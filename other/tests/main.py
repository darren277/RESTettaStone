""""""
import sys
import enum
import time
import requests

from other.populate_pg_db import create_table, drop_table

_id = 1

class HTTP(enum.Enum):
    GET = 'GET'
    POST = 'POST'
    PUT = 'PUT'
    DELETE = 'DELETE'
    PATCH = 'PATCH'
    OPTIONS = 'OPTIONS'
    HEAD = 'HEAD'
    TRACE = 'TRACE'
    CONNECT = 'CONNECT'

    def upper(self):
        return self.value.upper()


class Tester:
    def __init__(self, host: str, port: int, name: str, endpoint: str, method: HTTP, assertions: callable, data: dict = None, debug: bool = True):
        self.host = host
        self.port = port
        self.name = name
        self.endpoint = endpoint
        self.method = method
        self.assertions = assertions
        self.data = data
        self.debug = debug

    def test(self):
        global _id

        url = f"http://{self.host}:{self.port}/{self.name}{self.endpoint.format(_id=_id)}"
        if self.debug: print(f"---- Testing {url} with method {self.method} and data {self.data}...")
        if self.data:
            r = requests.request(self.method, url, json=self.data)
        else:
            r = requests.request(self.method, url)
        if self.name == 'firebaseapp' and self.method == HTTP.POST:
            _id = r.json().get('id')
            print("SETTING FIREBASE APP ID", _id)
        if self.debug:
            try:
                print(f"---- Response ({r.status_code}): {r.json()}")
            except:
                print(f"---- Response ({r.status_code}): {r.text}")
        for assertion in self.assertions:
            if self.debug: print(f"---- Running assertion {assertion}...")
            if not assertion(r): return False
        return True


def run_tests():
    global _id

    DELAY_TIME = 0.1

    args = sys.argv

    if len(args) != 4:
        print("\x1b[31;1m"+"Please provide the host and port of the nginx server."+"\x1b[0m")
        return

    nginx_host = args[2]
    nginx_port = int(args[3])
    try:
        debug = bool(args[4])
    except:
        debug = False

    print("\x1b[32;20m"+"Running tests..."+f"Nginx host: {nginx_host}. Nginx port: {nginx_port}."+"\x1b[0m")

    endpoint = '/users'
    method = HTTP.GET
    assertions = [lambda r: r.status_code == 200, lambda r: r.json()[0].get('email') == 'test_email1@testing.com']
    data = None

    test_get_many = Tester(host=nginx_host, port=nginx_port, name='', endpoint=endpoint, method=method, assertions=assertions, data=data, debug=debug)

    endpoint = '/users/{_id}'
    method = HTTP.GET
    assertions = [lambda r: r.status_code == 200, lambda r: r.json().get('email') == 'test_email1@testing.com']
    data = None

    test_get_one = Tester(host=nginx_host, port=nginx_port, name='', endpoint=endpoint, method=method, assertions=assertions, data=data, debug=debug)

    endpoint = '/users'
    method = HTTP.POST
    assertions = [lambda r: r.status_code == 200]
    data = {'email': 'test_email1@testing.com'}

    test_post = Tester(host=nginx_host, port=nginx_port, name='', endpoint=endpoint, method=method, assertions=assertions, data=data, debug=debug)

    endpoint = '/users/{_id}'
    method = HTTP.PUT
    assertions = [lambda r: r.status_code == 200]
    data = {'email': 'test_email1b@testing.com'}

    test_update = Tester(host=nginx_host, port=nginx_port, name='', endpoint=endpoint, method=method, assertions=assertions, data=data, debug=debug)

    endpoint = '/users/{_id}'
    method = HTTP.PUT
    assertions = [lambda r: r.status_code == 404]
    data = {'email': 'test_email1b@testing.com'}

    test_update_not_found = Tester(host=nginx_host, port=nginx_port, name='', endpoint=endpoint, method=method, assertions=assertions, data=data, debug=debug)

    endpoint = '/users/{_id}'
    method = HTTP.DELETE
    assertions = [lambda r: r.status_code == 200]
    data = None

    test_delete = Tester(host=nginx_host, port=nginx_port, name='', endpoint=endpoint, method=method, assertions=assertions, data=data, debug=debug)

    tests = [
        # This order is logical for the above tests...
        # We first POST one in order to create an initial user for the next few tests.
        # Then we test GET one and GET many to ensure that the user was created.
        # Then we test PUT to update the user.
        # Then we test DELETE to delete the user.
        # Then we test PUT again but with the recently deleted user to ensure that the user is not found.
        # NOTE that this requires a freshly cleaned database.
        test_post,
        test_get_one,
        test_get_many,
        test_update,
        test_delete,
        test_update_not_found
    ]

    for app in [
            'actixapp',
            'aspnetapp',
            'bashapp',
            'bunapp',
            'crowapp',
            'djangoapp',
            'fatfreeapp',
            'firebaseapp',
            'flaskapp',
            'fsharpapp',
            'goapp',
            'http4kapp',
            'hunchentootapp',
            'laravelapp',
            'luaapp',
            'nodeapp',
            'pascalapp',
            'perlapp',
            'phoenixapp',
            'phpapp',
            'playapp',
            'prologapp',
            'railsapp',
            'rocketapp',
            'shelfapp',
            'spockapp',
            'springbootapp',
            'swiftapp',
            'symfonyapp',
            'tomcatapp',
            'vibeapp',
            'zigapp'
        ]:
        print("\x1b[32;20m"+f"Testing {app}..."+'\x1b[0m')
        print("\x1b[32;20m"+"Dropping table (users)"+'\x1b[0m')
        drop_table('users')
        print("\x1b[32;20m"+"Creating table (users)"+'\x1b[0m')
        create_table('users')

        for test in tests:
            print("\x1b[32;20m"+f"Testing {test.endpoint} ({test.method})..."+'\x1b[0m')

            time.sleep(DELAY_TIME)
            test.name = app
            try:
                assert test.test()
                print("\x1b[32;20m"+f"{app} test passed for endpoint {test.endpoint.format(_id=_id)} ({test.method})."+'\x1b[0m')
            except AssertionError:
                print("\x1b[31;1m"+f"{app} test failed for endpoint {test.endpoint.format(_id=_id)} ({test.method})."+"\x1b[0m")
            except KeyError:
                print("\x1b[31;1m"+f"{app} test failed [KEY ERROR] for endpoint {test.endpoint.format(_id=_id)} ({test.method})."+"\x1b[0m")


if __name__ == '__main__':
    run_tests()
