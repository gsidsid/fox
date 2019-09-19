import json
import flask

from flask import request
from flask_dropzone import Dropzone

application = flask.Flask(__name__)
dropzone = Dropzone(application)

@application.route('/')
def serve():
    return flask.render_template("index.html")

@application.route('/index.html')
def returnHome():
    return flask.render_template("index.html")

@application.route('/uploads', methods=['GET', 'POST'])
def upload():

    if request.method == 'POST':
        f = request.files.get('file')
        f.save(os.path.join('files', f.filename))

    return 'upload template'

@application.route('/how.html')
def how():
    return flask.render_template("how.html")

if __name__ == '__main__':
    import os
    port = int(os.environ.get('PORT', 33507))
    application.run(host='0.0.0.0',port=port)