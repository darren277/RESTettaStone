""""""
import json
from dotenv import dotenv_values

config = dotenv_values(".env")

#host = config.get('PG_HOST')
host = '127.0.0.1'

PG_URL = f"postgres://{config.get('PG_USER')}:{config.get('PG_PASS')}@{host}:{config.get('PG_PORT')}/postgres"

from sqlalchemy import create_engine, text

engine = create_engine(PG_URL.replace('postgres://', 'postgresql://'))
#print(engine.table_names())

def create_table(table_name: str):
    with engine.connect() as connection:
        connection.execute(text(f"CREATE TABLE {table_name} (id serial PRIMARY KEY, name varchar, email varchar);"))

def insert_data(table_name: str, name: str, email: str):
    with engine.connect() as connection:
        connection.execute(text(f"INSERT INTO {table_name} (name, email) VALUES ('{name}', '{email}');"))


def select_data(table_name: str):
    with engine.connect() as connection:
        result = connection.execute(text(f"SELECT * FROM {table_name}"))
    for row in result:
        print(row)

def drop_table(table_name: str):
    ## NO LONGER WORKS AFTER UPDATE!
    with engine.connect() as connection:
        connection.execute(text(f"DROP TABLE IF EXISTS {table_name}"))


try:
    create_table('users')
except:
    pass


def add_user(name: str, email: str):
    insert_data('users', name, email)

def add_users():
    pg_data = json.loads(open('other/pg_data.json').read())

    for user in pg_data:
        add_user(user['name'], user['email'])

add_users()
