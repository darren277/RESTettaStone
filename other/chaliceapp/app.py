""""""
import os

from chalice import Chalice
import boto3 as boto3

app = Chalice(app_name="chaliceapp")

@app.route("/api/users")
def index():
    db = boto3.resource('dynamodb')
    table = db.Table(os.environ['USERS_TABLE'])
    users = [dict(id=item['ID'], email=item['email']) for item in table.scan().get('Items', [])]
    return users
