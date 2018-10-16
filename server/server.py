from flask import Flask, jsonify, g, url_for, send_file
import sqlite3
import geojson
from datetime import datetime as dt
from functools import wraps
from flask import request, Response
import json
import csv
import os.path
import io
from time import sleep
import subprocess
from shapely.geometry import shape
from math import isnan
from werkzeug.utils import secure_filename
from tempfile import mkdtemp,NamedTemporaryFile
from shutil import rmtree
import geopandas


app = Flask(__name__)

# Configuration routines
app.config.from_object(__name__)
configfile = "/srv/modis-snow-server/config.R"

download_stdout = None

def load_config(configfile,flaskapp):
    required = ["MODIS_DATASTORAGE", "DATASTORAGE_LOC", "DATABASE_LOC","DOWNLOAD_TRIGGER","APP_USER","APP_PW"]
    input = dict()
    configvariables = dict()
    with open(configfile) as f:
        content = f.readlines()
    for line in content:
        x = line.split("=")
        try:
            input[x[0].strip()] = x[1].strip()
        except:
            pass
    for item in required:
        try:
            configvariables[item] = input[item].strip('"')
        except KeyError as Error:
            print("The configfile does not contain an entry for %s" %item)
            raise(Error)
    flaskapp.config.update(configvariables)
    return None

load_config(configfile, app)

# Database routines
def get_db():
    db = getattr(g, '_database', None)
    if db is None:
        db = sqlite3.connect(app.config['DATABASE_LOC'])
    return db

@app.teardown_appcontext
def close_connection(exception):
    db = getattr(g, '_database', None)
    if db is not None:
        db.close()

def make_dicts(cursor, row):
    return dict((cursor.description[idx][0], value)
                for idx, value in enumerate(row))

def query_db(query, args=(), one=False):
    cur = get_db().execute(query, args)
    rv = cur.fetchall()
    out = list()
    for row in rv:
        out.append(make_dicts(cur,row))
    cur.close()
    return (out[0] if out else None) if one else out

# Authentification routines
def check_auth(username, password):
    """This function is called to check if a username /
    password combination is valid.
    """
    return username == app.config['APP_USER'] and password == app.config['APP_PW']

def authenticate():
    """Sends a 401 response that enables basic auth"""
    raise Error('Could not verify your access level for that URL. You have to login with proper credentials', status_code=401)

def requires_auth(f):
    @wraps(f)
    def decorated(*args, **kwargs):
        auth = request.authorization
        if not auth or not check_auth(auth.username, auth.password):
            return authenticate()
        return f(*args, **kwargs)
    return decorated

def shp2json(shapefilename):
    geodataframe = geopandas.read_file(shapefilename)
    geodataframe_4326 = geodataframe.to_crs({'init': 'epsg:4326'})
    tempfile = NamedTemporaryFile();tempfile.close()
    geodataframe_4326.to_file(tempfile.name,driver='GeoJSON')
    with open(tempfile.name) as f:
        gj = geojson.load(f)
    return(gj)

def is_deleted(id):
    res = query_db('select deletion from settings where ID = ?', [id])
    if len(res)==0:
        return True
    elif res[0]['deletion'] == 0:
        return False
    else:
        return True

class Error(Exception):
    def __init__(self, message, status_code, payload=None):
        Exception.__init__(self)
        self.message = message
        self.status_code = status_code
        self.payload = payload

    def to_dict(self):
        rv = dict(self.payload or ())
        rv['message'] = self.message
        return rv

def data_processor_status():
    status = 'idle'
    content = None
    uptime = None
    for file in os.listdir(app.config['DATASTORAGE_LOC']):
        if file.endswith(".LOCKED"):
            status = 'running'
            fullpath = os.path.join(app.config['DATASTORAGE_LOC'],file)
            reader = open(fullpath, 'r')
            content = reader.read()
            starttime = dt.strptime(file.replace(".LOCKED",""), "%Y-%m-%d %H:%M:%S")
            uptime = dt.today()-starttime
            break
    return {'status': status, 'output': content, 'uptime': str(uptime)}

@app.errorhandler(Error)
def handle_error(error):
    response = jsonify(error.to_dict())
    response.status_code = error.status_code
    return response

@app.errorhandler(404)
def handle_error(error):
    response = jsonify({'message': 'Not Found'})
    response.status_code = 404
    return response

@app.errorhandler(500)
def handle_error(error):
    response = jsonify({'message': 'Internal Server Error'})
    response.status_code = 500
    return response

@app.errorhandler(401)
def handle_error(error):
    response = jsonify({'message': 'Unauthorized'})
    response.status_code = 401
    return response

@app.errorhandler(405)
def handle_error(error):
    response = jsonify({'message': 'Method Not Allowed'})
    response.status_code = 405
    return response

@app.route('/', methods=['GET'])
def status():
    # TODO: Add filesystem usage to status info?
    return jsonify({"status" : "OK"})

@app.route('/data_processor', methods=['PUT'])
@requires_auth
def data_processor_trigger():
    response = data_processor_status()
    if response['status']=='idle':
        trigger = app.config['DOWNLOAD_TRIGGER']
        try:
            process=subprocess.Popen(trigger, shell=True)
            waiting=0
            while response['status']=='idle':
                response = data_processor_status()
                if waiting < 5:
                    sleep(0.5); waiting=waiting+0.5
                else:
                    raise Exception
            response = jsonify(response)
            response.status_code = 202
            return response
        except:
            raise Error(process, status_code=500)
    else:
        raise Error('data processor is already running. Try again later', status_code=409)

@app.route('/data_processor', methods=['GET'])
def response_data_processor_status():
    response = data_processor_status()
    return jsonify(response)

@app.route('/catchments', methods=['GET'])
def list_catchments():
    entries = query_db('select ID,name,last_obs_ts,last_obs_gtif from settings where deletion = 0')
    for i, entry in enumerate(entries):
        entries[i].pop('geojson', None)
        entries[i].update({"href":url_for('show_catchment',id=entry["ID"])})
        entries[i].update({"timeseries": {'href' : url_for('list_timeseries', id=entry["ID"])}})
        entries[i].update({"geotiff": {'href' : url_for('list_geotiff', id=entry["ID"])}})
        entries[i].update({"geojson": {'href' : url_for('show_geojson', id=entry["ID"])}})
    return jsonify(entries)

@app.route('/catchments', methods=['POST'])
@requires_auth
def add_catchment():
    if not request.json:
        raise Error('request must be of type json', status_code=400)

    input = request.json
    if not set(('name', 'geojson')) <= set(input.keys()):
        raise Error('request must include a json with values for the keys name (str) and geojson (geojson)', status_code=400)
    else:
        name = input['name']
        region_geojson = input['geojson']
        if isinstance(region_geojson,dict):
            region_geojson = json.dumps(region_geojson)
        if isinstance(region_geojson,unicode):
            region_geojson = json.dumps(json.loads(region_geojson))

    try:
        geojson_obj = geojson.loads(region_geojson)
    except:
        raise Error('the provided geojson does not describe a valid spatial object', status_code=400)
    if len(geojson_obj['features']) != 1:
        raise Error('the provided geojson has more than one feature. Only one feature of type polygon is allowed', status_code=400)
    elif geojson_obj['features'][0]['geometry'] is None:
        raise Error('the provided geojson must contain a feature of type polygon.', status_code=400)
    elif geojson_obj['features'][0]['geometry']['type'] not in ['Polygon','MultiPolygon']:
        raise Error('the provided geojson must contain a feature of type Polygon or MultiPolygon.', status_code=400)
    elif 'crs' not in geojson_obj.keys():
        raise Error('the provided geojson does not have a projection', status_code=400)
    elif str(geojson_obj['crs']['properties']['name']) != 'urn:ogc:def:crs:OGC:1.3:CRS84':
        raise Error('the geojson must have the following projection: urn:ogc:def:crs:OGC:1.3:CRS84', status_code=400)

    conn = get_db()
    cursor = conn.cursor()
    result = query_db('select geojson from settings where ID = 1')
    master_geojson = json.loads(result[0]['geojson'])
    master_shape = shape(master_geojson['features'][0]['geometry'])
    subregion_shape = shape(geojson_obj['features'][0]['geometry'])
    difference_shape = subregion_shape.difference(master_shape)
    if difference_shape.area > 0:
        raise Error('the provided geojson polygon does not overlap with the masterregion. The server can not support this request.', status_code=400)

    if "store_length" in input.keys():
        try:
            store_length = int(input['store_length'])
            if store_length < 200: raise(Exception)
        except:
            raise Error('store_length must be an integer > 199',  status_code=400)

    else:
        store_length = 365

    if "elev_split" in input.keys():
        try:
            elev_split = int(input['elev_split'])
            if elev_split < 1: raise(Exception)
        except:
            raise Error('elev_split must be an integer > 0',  status_code=400)
    else:
        elev_split = None

    if "earliestdate" in input.keys():
        try:
            earliestdate = str(input['earliestdate'])
            dt.strptime(earliestdate, '%Y-%m-%d')
        except:
            raise Error('earliestdate must be a string with datetime format YYYY-MM-DD', status_code=400)
    else:
        earliestdate = None


    if "latestdate" in input.keys():
        try:
            latestdate = str(input['latestdate'])
            dt.strptime(latestdate, '%Y-%m-%d')
        except:
            raise Error('latestdate must be a string with datetime format YYYY-MM-DD', status_code=400)
    else:
        latestdate = None

    conn = get_db()
    cursor = conn.cursor()
    cursor.execute('insert into settings (name, geojson, store_length, elev_split, earliestdate, latestdate) values (?,?,?,?,?,?)', (name, region_geojson, store_length, elev_split, earliestdate, latestdate))
    new_id = cursor.lastrowid
    conn.commit()
    r = show_catchment(new_id)
    return Response(response=r.response,status=201,headers=r.headers,mimetype=r.mimetype,content_type=r.content_type)

@app.route('/catchments/<id>', methods=['GET'])
def show_catchment(id):
    entry = query_db('select * from settings where ID = ? and deletion = 0',[id])
    if len(entry) == 0:
        raise Error('there is no catchment with the requested id', status_code=404)
    else:
        entry = entry[0]
        entry.pop('geojson', None)
        entry.update({"timeseries": {'href' : url_for('list_timeseries', id=entry["ID"])}})
        entry.update({"geotiff": {'href' : url_for('list_geotiff', id=entry["ID"])}})
        entry.update({"geojson": {'href' : url_for('show_geojson', id=entry["ID"])}})
        return jsonify(entry)

@app.route('/catchments/<id>', methods=['DELETE'])
def tag_catchment4deletion(id):
    if is_deleted(id):
        raise Error('there is no catchment with the requested id', status_code=404)
    elif int(id) is not 1:
        conn = get_db()
        cursor = conn.cursor()
        cursor.execute('update settings set deletion = 1 where ID = ?',[id])
        conn.commit()
        return ('', 204)
    else:
        raise Error('... and even if I were not, to ask for the deletion of the masterregion, that is too hot.', status_code=418)

@app.route('/catchments/<id>/timeseries', methods=['GET'])
def list_timeseries(id):
    if not is_deleted(id):
        entries = query_db('select ID,catchmentid,min_elev,max_elev,elev_zone from timeseries where catchmentid = ?', [id])
        for i, entry in enumerate(entries):
            entries[i].update({"href": url_for('show_timeseries', id=entry["ID"], catchmentid=entry['catchmentid'])})
        return jsonify(entries)
    else:
        raise Error('there is no catchment with the requested id', status_code=404)

@app.route('/catchments/<catchmentid>/timeseries/<id>', methods=['GET'])
def show_timeseries(id,catchmentid):
    if is_deleted(catchmentid):
        raise Error('there is no catchment with the requested id', status_code=404)
    path = query_db('select filepath from timeseries where ID = ? and catchmentid = ?', [id, catchmentid])
    if len(path)==0:
        raise Error('there is no timeseries with the requested id', status_code=404)

    fullpath = os.path.join(app.config['DATASTORAGE_LOC'],path[0]['filepath'])

    argkeys = request.args.keys()
    if len(argkeys)- argkeys.count('from') - argkeys.count('to') > 0:
        raise Error('the server could not understand all of the provided query parameters. Use only from and to.', status_code=400)
    try:
        with open(fullpath) as csvfile:
            reader = csv.reader(csvfile)
            header = reader.next()
            datepos = header.index('date')
            valuepos = 1 - datepos
            datadict = {rows[datepos]: None if isnan(float(rows[valuepos])) else float(rows[valuepos]) for rows in reader}
    except:
        raise Error('the server failed to locate the requested timeseries', status_code=500)

    dates = [dt.strptime(key, '%Y-%m-%d') for key in datadict.keys()]
    if 'from' in request.args.keys():
        try:
            begindate = dt.strptime(request.args['from'], '%Y-%m-%d')
            dates = [date for date in dates if date >= begindate]
        except:
            raise Error('wrong date format. use <YYYY-MM-DD', status_code=400)

    if 'to' in request.args.keys():
        try:
            enddate = dt.strptime(request.args['to'], '%Y-%m-%d')
            dates = [date for date in dates if date <= enddate]
        except:
            raise Error('wrong date format. use <YYYY-MM-DD', status_code=400)

    datadict = {date.strftime('%Y-%m-%d') : datadict[date.strftime('%Y-%m-%d')] for date in dates}
    return jsonify(datadict)

@app.route('/catchments/<id>/geotiffs', methods=['GET'])
def list_geotiff(id):
    if not is_deleted(id):
        entries = query_db('select ID,catchmentid,date from geotiffs where catchmentid = ?', [id])

        begindate = dt.min
        enddate = dt.max
        argkeys = request.args.keys()
        if len(argkeys) - argkeys.count('from') - argkeys.count('to') > 0:
            raise Error('the server could not understand all of the provided query parameters. Use only from and to.',
                        status_code=400)

        if 'from' in request.args.keys():
            try:
                begindate = dt.strptime(request.args['from'], '%Y-%m-%d')
            except:
                raise Error('wrong date format. use <YYYY-MM-DD', status_code=400)

        if 'to' in request.args.keys():
            try:
                enddate = dt.strptime(request.args['to'], '%Y-%m-%d')
            except:
                raise Error('wrong date format. use <YYYY-MM-DD', status_code=400)

        selected = list()

        for i, entry in enumerate(entries):
            entries[i].update({'href': url_for('show_geotiff', catchmentid=id, id=entry['ID'])})
            selected.append(i) if begindate<=dt.strptime(entries[i]['date'], '%Y-%m-%d')<=enddate else False

        return jsonify([entries[i] for i in selected])
    else:
        raise Error('there is no catchment with the requested id', status_code=404)


@app.route('/catchments/<catchmentid>/geotiffs/<id>', methods=['GET'])
def show_geotiff(id, catchmentid):
    if is_deleted(catchmentid):
        raise Error('there is no catchment with the requested id', status_code=404)
    path = query_db('select date,filepath from geotiffs where ID = ? and catchmentid = ?', [id, catchmentid])
    if len(path) == 0:
        raise Error('there is no geotiff with the requested id', status_code=404)

    fullpath = os.path.join(app.config['DATASTORAGE_LOC'],path[0]['filepath'])

    try:
        with open(fullpath, 'rb') as geotiff:
            return send_file(
                io.BytesIO(geotiff.read()),
                attachment_filename=path[0]['date']+'.tif',
                mimetype='image/geotiffint16'
            )
    except:
        raise Error('the server failed to locate the requested geotiff', status_code=500)

@app.route('/catchments/<id>/geojson', methods=['GET'])
def show_geojson(id):
    entry = query_db('select * from settings where ID = ? and deletion = 0', [id])
    if len(entry) == 0:
        raise Error('there is no catchment with the requested id', status_code=404)
    else:
        geojson_obj = json.loads(entry[0]['geojson'])
    return jsonify(geojson_obj)


# Shapefile Converter


UPLOAD_FOLDER = None
ALLOWED_EXTENSIONS = set(['shp','shx','dbf','prj'])

app.config['UPLOAD_FOLDER'] = UPLOAD_FOLDER

def allowed_file(filename):
    return '.' in filename and \
           filename.rsplit('.', 1)[1].lower() in ALLOWED_EXTENSIONS

@app.route('/shapefile2json', methods=['GET','POST'])
def shapefile2json():
    shp_dict = dict()
    if request.method == 'POST':
        # check if the post request has the file part
        if 'files' not in request.files:
            raise Error('there is no files attribute in the the request',status_code=400)
        filelist = request.files.getlist('files')
        tempdir = mkdtemp()
        for file in filelist:
            filename = secure_filename(file.filename)
            if not hasattr(file, 'filename') or str(file.filename) == '':
                raise Error('filenames are empty', status_code=400)
            elif not allowed_file(filename):
                raise Error('file extension is not allowed. Only shp,shx,dbf,prj are allowed.', status_code=400)
            else:
                path = os.path.join(tempdir,filename)
                file.save(path)
                shp_dict.update({filename.rsplit('.', 1)[1].lower(): path})

        if set(['shx', 'shp', 'dbf','prj']) == set(shp_dict.keys()):
            try:
                geojson = shp2json(shp_dict['shp'])
                rmtree(tempdir, ignore_errors=True)
            except:
                raise Error('there was an error converting the provided shapefile data.', status_code=500)
        else:
            raise Error('One of the following files is missing: shp,shx,prj or dbf', status_code=400)
        return jsonify(geojson)
    else:
        return '''
            <!doctype html>
            <title>shapefile 2 geojson converter</title>
            <h2>convert shapefile to geojson:</h2>
            <form method=post enctype=multipart/form-data>
              <p>shp: <input type=file name=files><br>shx: <input type=file name=files><br>dbf: <input type=file name=files><br>prj : <input type=file name=files>
                 <input type=submit value=Upload><br><br># with curl: curl -i -X POST -F files=@&lt;path.shp&gt; -F files=@&lt;path.shx&gt; -F files=@&lt;path.dbf&gt -F files=@&lt;path.prj&gt; &lt;this:url&gt;

            </form>
            '''

if __name__ == '__main__':
    app.run(debug=True)
