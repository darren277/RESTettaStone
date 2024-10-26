""""""
import os
import json

from flask_sqlalchemy import *
from sqlalchemy import *
from sqlalchemy.ext.declarative import DeferredReflection, declarative_base


PORT = os.environ.get('PORT', 5000)

PG_USER = os.environ.get('PG_USER', 'postgres')
PG_PASS = os.environ.get('PG_PASS', 'password')
PG_HOST = os.environ.get('PG_HOST', 'localhost')
PG_PORT = os.environ.get('PG_PORT', 5432)
PG_DB = os.environ.get('PG_DB', 'postgres')


class Reflected(DeferredReflection):
    __abstract__ = True

Base = declarative_base()


class User(Reflected, Base):
    __tablename__ = 'users'

    id = Column(Integer, primary_key=True)
    email = Column(Text)

    def __init__(self, email: str):
        self.email = email

    def json(self):
        return {
            'id': self.id,
            'email': self.email
        }



db = SQLAlchemy()

from flask import Flask
app = Flask(__name__)

app.config['SQLALCHEMY_DATABASE_URI'] = f'postgresql://{PG_USER}:{PG_PASS}@{PG_HOST}:{PG_PORT}/{PG_DB}'

db.init_app(app)


with app.app_context():
    Base.metadata.create_all(db.engine)
    Reflected.prepare(db.engine)
    Base.query = db.session.query_property()



@app.route('/')
def hello_world():
    return 'Hello, World!'


@app.route('/users')
def users():
    users = User.query.all()
    data = {'users': [user.json() for user in users]}
    response = app.response_class(response=json.dumps(data['users']), status=200, mimetype='application/json')
    return response


if __name__ == '__main__':
    app.run(host='0.0.0.0', port=PORT)



