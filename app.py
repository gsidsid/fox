import json
import flask
import subprocess
import time
import docker
import re
import pprint
import os
import sys

from flask import request
from flask_dropzone import Dropzone
from collections import OrderedDict

application = flask.Flask(__name__)
application.config['DROPZONE_ALLOWED_FILE_CUSTOM']=True
application.config['DROPZONE_ALLOWED_FILE_TYPE']='.ml'
application.config['DROPZONE_MAX_FILE_SIZE']=1

dropzone = Dropzone(application)

compilation_log_result = ""
test_log_result = ""

tr = None

class Case:
    def __init__(self, rawIEO):
        self.case = dict()
        self.case['input'] = rawIEO[rawIEO.index("INPUT:")+6:rawIEO.index("EXPECTED:")]
        self.case['expected'] = rawIEO[rawIEO.index("EXPECTED:")+9:rawIEO.index("OUTPUT:")]
        self.case['output'] = rawIEO[rawIEO.index("OUTPUT:")+7:]

    def __str__(self):
        res = ""
        res += "INPUT: " + str(self.case['input']) + "\n"
        res += "EXPECTED: " + str(self.case['expected']) + "\n"
        res += "OUTPUT: " + str(self.case['output'])
        return res

class Cases:
    def __init__(self, pfarray):
        self.cases = dict()
        self.cases['passed'] = []
        self.cases['failed'] = []
        for i in range(len(pfarray)):
            if pfarray[i] == 'Passed':
                self.cases['passed'].append(Case(pfarray[i+1]))
            elif pfarray[i] == 'Failed':
                self.cases['failed'].append(Case(pfarray[i+1]))

    def __str__(self):
        res = ""
        for case in self.cases['passed']:
            res += "PASS\n"
            res += "---------------\n"
            res += str(case) + "\n\n"
        for case in self.cases['failed']:
            res += "FAIL\n"
            res += "---------------\n"
            res += str(case) + "\n\n"

        return res

    def serialize(self):
        return {
            'passed': [case.case for case in self.cases['passed'] ],
            'failed': [case.case for case in self.cases['failed'] ]
        }
        

class TestResult:
    def __init__(self, COMP, TEST):
        self.filename = "*.ml"
        self.compiles = False
        self.tests = False
        self.suites = OrderedDict()
        self.err = None
        self.skips = 0

        if len(COMP) == 0:
            self.compiles = True
            self.tests = True
            self.filename = str(TEST[10:TEST.index("Starting")])
            raw_suites = TEST.split('--------------------------------------------------------------------------------')[:-1]
            for rs in raw_suites:
                it = rs.index('\'')
                it_e = rs.find(' ',it)
                suitekey = str(rs[it+1:it_e])
                suiteresults = re.split('(Passed|Failed)', rs)
                suiteresults = suiteresults[1:]
                self.suites[suitekey] = Cases(["".join(sr.encode('ascii', 'ignore').splitlines()).replace('\x1b[32m','').replace('\x1b[0m','').replace('\\n','').replace('\t\\','').replace('\t','') for sr in suiteresults])
        else:
            self.err = COMP

        for key, value in self.suites.items():
            if "Skipped" in str(value):
                self.skips += 1

        print("Filename: " + self.filename)
        print("Skipped " + str(self.skips) + " tests.")
        print("Compiled with error: " + str(self.err))

    def __str__(self):
        res = ""
        for key, value in self.suites.items():
            res += key + "\n"
            res += "===============\n"
            val = ""
            if "Skipped" in str(value):
                val = str(value)[:str(value).index("Skipped")] + str(value)[str(value).index("parse output")+13:]
            else:
                val = str(value)
            res += val + "\n\n"
        return res

    def serialize(self):
        res = []
        for key, value in self.suites.items():
            res.append({
                key: value.serialize()
            })
        return res

@application.route('/mail/<fname>')
def mail(fname):
    return flask.redirect("mailto:riccardo.pucella@olin.edu?subject=fname%20SSubmission")

@application.route('/')
def serve():
    return flask.render_template("index.html")

@application.route('/index.html')
def returnHome():
    return flask.render_template("index.html")
    
@application.route('/uploads', methods=['GET', 'POST'])
def upload():
    print("Upload phase")
    if request.method == 'POST':
        f = request.files.get('file')
        f.save(os.path.join(os.getcwd()+'/files', f.filename))
    return flask.Response(status=200)

@application.route('/compile/<filename>')
def compiler(filename):
    client = docker.from_env()
    compilation_log_result = ""
    test_log_result = ""
    try:
        print("Compilation phase")
        print(os.listdir(os.getcwd()+"/files"))
        comp_container = client.containers.run('akabe/ocaml:ubuntu16.04_ocaml4.07.0',volumes={os.getcwd()+'/files':{'bind':'/files', 'mode': 'ro'},'/var/run/docker.sock':{'bind':'/var/run/docker.sock', 'mode': 'rw'}},command="ocaml /files/" + str(filename))
        compilation_log_result = comp_container
        return flask.Response(status=200)
    except Exception as e:
        compilation_log_result = str(e.message)
        os.remove(os.path.join('files',filename))
    return flask.Response(json.dumps({"error":compilation_log_result}), status=400, mimetype='application/json')

@application.route('/test/<filename>')
def tester(filename):
    client = docker.from_env()
    exec_container = client.containers.run('sidworld/fox',environment=["TARGET="+filename],volumes={os.getcwd()+'/files':{'bind':'/files', 'mode': 'ro'}})
    test_log_result = exec_container
    re.sub(r'\x1b(\[.*?[@-~]|\].*?(\x07|\x1b\\))', '', test_log_result)
    tr = TestResult(compilation_log_result, test_log_result)
    print(test_log_result)
    return flask.Response(json.dumps(tr.serialize()), status=200, mimetype='application/json')


@application.route('/how.html')
def how():
    return flask.render_template("how.html")

if __name__ == '__main__':
    import os
    port = int(os.environ.get('PORT', 33507))
    application.run(host='0.0.0.0',port=port)
