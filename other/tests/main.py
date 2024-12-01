""""""
import sys
import enum
import requests


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
    def __init__(self, host: str, port: int, name: str, endpoint: str, method: HTTP, assertions: callable, data: dict = None):
        self.host = host
        self.port = port
        self.name = name
        self.endpoint = endpoint
        self.method = method
        self.assertions = assertions
        self.data = data

    def test(self):
        url = f'http://{self.host}:{self.port}/{self.name}{self.endpoint}'
        if self.data:
            r = requests.request(self.method, url, json=self.data)
        else:
            r = requests.request(self.method, url)
        for assertion in self.assertions:
            if not assertion(r): return False
        return True


def run_tests():
    args = sys.argv

    if len(args) != 3:
        print("\x1b[31;1m"+"Please provide the host and port of the nginx server."+"\x1b[0m")
        return

    nginx_host = args[1]
    nginx_port = int(args[2])

    _id = 1

    endpoint = '/users'
    method = HTTP.GET
    assertions = [lambda r: r.status_code == 200, lambda r: r.json()[0].get('email') == 'test_email1@testing.com']
    data = None

    test_get_many = Tester(host=nginx_host, port=nginx_port, name='', endpoint=endpoint, method=method, assertions=assertions, data=data)

    endpoint = '/users'
    method = HTTP.GET
    assertions = [lambda r: r.status_code == 200, lambda r: r.json()[0].get('email') == 'test_email1@testing.com']
    data = None

    test_get_one = Tester(host=nginx_host, port=nginx_port, name='', endpoint=endpoint, method=method, assertions=assertions, data=data)

    endpoint = '/users'
    method = HTTP.POST
    assertions = [lambda r: r.status_code == 200]
    data = {'email': 'test_email1@testing.com'}

    test_post = Tester(host=nginx_host, port=nginx_port, name='', endpoint=endpoint, method=method, assertions=assertions, data=data)

    endpoint = f'/users/{_id}'
    method = HTTP.PUT
    assertions = [lambda r: r.status_code == 200]
    data = {'email': 'test_email1b@testing.com'}

    test_update = Tester(host=nginx_host, port=nginx_port, name='', endpoint=endpoint, method=method, assertions=assertions, data=data)

    endpoint = f'/users/{_id}'
    method = HTTP.PUT
    assertions = [lambda r: r.status_code == 404]
    data = {'email': 'test_email1b@testing.com'}

    test_update_not_found = Tester(host=nginx_host, port=nginx_port, name='', endpoint=endpoint, method=method, assertions=assertions, data=data)

    endpoint = f'/users/{_id}'
    method = HTTP.DELETE
    assertions = [lambda r: r.status_code == 200]
    data = None

    test_delete = Tester(host=nginx_host, port=nginx_port, name='', endpoint=endpoint, method=method, assertions=assertions, data=data)

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
            #'actixapp', 'aspnetapp', 'bunapp', 'crowapp', 'djangoapp',
            #'fatfreeapp', 'firebaseapp', 'flaskapp', 'fsharpapp', 'goapp',
            #'laravelapp', 'luaapp', 'nodeapp', 'perlapp', 'phpapp', 'playapp',
            #'prologapp', 'railsapp', 'rocketapp', 'springbootapp', 'swiftapp',
            #'symfonyapp', 'tomcatapp', 'vibeapp', 'zigapp'
            'flaskapp'
        ]:
        for test in tests:
            import time
            time.sleep(5)
            test.name = app
            try:
                assert test.test()
                print("\x1b[32;20m"+f"{app} test passed for endpoint {test.endpoint} ({test.method})."+'\x1b[0m')
            except AssertionError:
                print("\x1b[31;1m"+f"{app} test failed for endpoint {test.endpoint} ({test.method})."+"\x1b[0m")
            except KeyError:
                print("\x1b[31;1m"+f"{app} test failed [KEY ERROR] for endpoint {test.endpoint} ({test.method})."+"\x1b[0m")


if __name__ == '__main__':
    run_tests()
