""""""
import json
from flask import Flask, jsonify


app = Flask(__name__)


@app.route('/')
def hello_world():
    return open('other/utils/visuals/visuals.html').read()

@app.route('/visuals.js')
def visuals_js():
    return open('other/utils/visuals/visuals.js').read()

@app.route('/data.json')
def data_json():
    return jsonify(json.loads(open('other/utils/visuals/data.json').read()))


if __name__ == '__main__':
    app.run()
