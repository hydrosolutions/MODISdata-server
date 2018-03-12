import rpy2.robjects as robjects
from flask import Flask
app = Flask(__name__)



r_source = robjects.r['source']
r_source("/home/jules/Desktop/Hydromet/MODISsnow_server/config.R")


@app.route('/')
def hello_world():
    return 'Server is running'

@app.route('/getdata')
def getdata(ID):
    ts_file = buildpath(ID)
    ts = csv.reader(ts_file, delimiter=',')
    return 'The about page'