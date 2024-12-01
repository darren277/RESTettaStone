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
    s = f"CREATE TABLE {table_name} (id serial PRIMARY KEY, name varchar, email varchar);"
    with engine.connect() as connection:
        connection.execute(text(s))
        connection.commit()

def insert_data(table_name: str, name: str, email: str):
    with engine.connect() as connection:
        connection.execute(text(f"INSERT INTO {table_name} (name, email) VALUES ('{name}', '{email}');"))
        connection.commit()

def select_data(table_name: str):
    with engine.connect() as connection:
        result = connection.execute(text(f"SELECT * FROM {table_name}"))
    for row in result:
        print(row)

def drop_table(table_name: str):
    with engine.connect() as connection:
        connection.execute(text(f"DROP TABLE IF EXISTS {table_name}"))
        connection.commit()


def add_user(name: str, email: str):
    insert_data('users', name, email)

def add_users():
    try:
        create_table('users')
    except Exception as e:
        print(e)
        if 'psycopg2.errors.DuplicateTable' in str(e):
            print('Table already exists... Dropping...')
            drop_table('users')
            create_table('users')
    try:
        pg_data = json.loads(open('other/pg_data.json').read())
    except FileNotFoundError:
        print('No file found... Trying root directory...')
        try:
            pg_data = json.loads(open('pg_data.json').read())
        except:
            print('No file found in root directory either... Exiting...')
            return
    for user in pg_data:
        add_user(user['name'], user['email'])

if __name__ == '__main__':
    print("ADDING USERS")
    add_users()
