from flask import Flask, jsonify, g, abort, url_for
import sqlite3
import shapefile
import geojson
from datetime import datetime as dt
from functools import wraps
from flask import request, Response, make_response
import json

app = Flask(__name__)

# Configuration routines
app.config.from_object(__name__)
configfile = "/home/jules/Desktop/Hydromet/MODISsnow_server/MODISsnow-server/downloader/examplefiles/config.R"
def load_config(configfile,flaskapp):
    required = ["MODIS_DATASTORAGE", "DATASTORAGE_LOC", "DATABASE_LOC","APP_USER","APP_PW"]
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
config = load_config(configfile, app)

# Database routines
def get_db():
    db = getattr(g, '_database', None)
    if db is None:
        db = g._database = sqlite3.connect(app.config['DATABASE_LOC'])
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
    return Response(
    'Could not verify your access level for that URL.\n'
    'You have to login with proper credentials', 401,
    {'WWW-Authenticate': 'Basic realm="Login Required"'})

def requires_auth(f):
    @wraps(f)
    def decorated(*args, **kwargs):
        auth = request.authorization
        if not auth or not check_auth(auth.username, auth.password):
            return authenticate()
        return f(*args, **kwargs)
    return decorated

def shp2gson(shapefilename):
    reader = shapefile.Reader(shapefilename)
    fields = reader.fields[1:]
    field_names = [field[0] for field in fields]
    buffer = []
    for sr in reader.shapeRecords():
        atr = dict(zip(field_names, sr.record))
        geom = sr.shape.__geo_interface__
        buffer.append(dict(type="Feature", \
                           geometry=geom, properties=atr))

        # write the GeoJSON file
    from json import dumps
    return({"type": "FeatureCollection","features": buffer})

def is_deleted(id):
    res = query_db('select deletion from settings where ID = ?', [id])
    if len(res)==0:
        return True
    elif res[0]['deletion'] == 0:
        return False
    else:
        return True

@app.errorhandler(404)
def not_found(error):
    return make_response(jsonify({'error': 'Not found'}), 404)

@app.route('/', methods=['GET'])
def status():
    # TODO: Add filesystem usage to status info?
    return jsonify({"status" : "OK"})

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
        abort(400, 'request must be of type json')

    input = request.json
    if not set(('name', 'geojson')) <= set(input.keys()):
        abort(400, 'request must include a json with values for the keys name (str) and geojson (geojson)')
    else:
        name = input['name']
        region_geojson = input['geojson']
        if isinstance(region_geojson,dict):
            region_geojson = json.dumps(region_geojson)
        if isinstance(region_geojson,unicode):
            region_geojson = json.dumps(json.loads(region_geojson))

    try:
        geojson_obj = geojson.loads(region_geojson)
        if len(geojson_obj['features']) != 1:
            abort(400, 'the provided geojson has more than one feature. Only one feature of type polygon is allowed')
        elif geojson_obj['features'][0]['geometry']['type'] is not 'Polygon':
            abort(400, 'the provided geojson must contain a feature of type polygon. Another type was found instead')
    except:
        abort(400, 'the provided geojson does not describe a valid spatial object')

    if "store_length" in input.keys():
        try:
            store_length = int(input['store_length'])
            if store_length < 1: raise(Exception)
        except:
            abort(400, 'store_length must be an integer > 0')
    else:
        store_length = 365

    if "elev_split" in input.keys():
        try:
            elev_split = int(input['elev_split'])
            if elev_split < 1: raise(Exception)
        except:
            abort(400, 'elev_split must be an integer > 0')
    else:
        elev_split = 500

    if "earliestdate" in input.keys():
        try:
            earliestdate = str(input['earliestdate'])
            dt.strptime(earliestdate, '%Y-%m-%d')
        except:
            abort(400, 'earliestdate must be a string with datetime format YYYY-MM-DD')
    else:
        earliestdate = None


    if "latestdate" in input.keys():
        try:
            latestdate = str(input['latestdate'])
            dt.strptime(latestdate, '%Y-%m-%d')
        except:
            abort(400, 'earliestdate must be a string with datetime format YYYY-MM-DD')
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
        abort(404, 'there is no catchment with the requested id')
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
        abort(404, 'there is no catchment with the requested id')
    elif int(id) is not 1:
        conn = get_db()
        cursor = conn.cursor()
        cursor.execute('update settings set deletion = 1 where ID = ?',[id])
        conn.commit()
        return ('', 204)
    else:
        abort(418,'... and even if I were not, to ask for the deletion of the masterregion, that is too hot.')

@app.route('/catchments/<id>/timeseries', methods=['GET'])
def list_timeseries(id):
    if not is_deleted(id):
        entries = query_db('select * from timeseries where ID = ?', [id])
        return jsonify(entries)
    else:
        abort(404, 'there is no catchment with the requested id')

@app.route('/catchments/<id>/geotiffs', methods=['GET'])
def list_geotiff(id):
    if not is_deleted(id):
        entries = query_db('select * from geotiffs where ID = ?', [id])
        return jsonify(entries)
    else:
        abort(404, 'there is no catchment with the requested id')

@app.route('/catchments/<id>/geojson', methods=['GET'])
def show_geojson(id):
    entry = query_db('select * from settings where ID = ? and deletion = 0', [id])
    if len(entry) == 0:
        abort(404, 'there is no catchment with the requested id')
    else:
        geojson_obj = json.loads(entry[0]['geojson'])
    return jsonify(geojson_obj)



if __name__ == '__main__':


    #conn = sqlite3.connect(sqlite_file)
    #c = conn.cursor()

    app.run(debug=True)