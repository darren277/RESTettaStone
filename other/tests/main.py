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
    def __init__(self, host: str, port: int, name: str, endpoint: str, method: HTTP, assertions: callable):
        self.host = host
        self.port = port
        self.name = name
        self.endpoint = endpoint
        self.method = method
        self.assertions = assertions

    def test(self):
        url = f'http://{self.host}:{self.port}/{self.name}{self.endpoint}'
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

    endpoint = '/users'
    method = HTTP.GET
    assertions = [lambda r: r.status_code == 200, lambda r: r.json()[0].get('email') == 'test_email1@testing.com']

    for app in [
            'actixapp', 'aspnetapp', 'bunapp', 'crowapp', 'djangoapp',
            'fatfreeapp', 'firebaseapp', 'flaskapp', 'fsharpapp', 'goapp',
            'laravelapp', 'luaapp', 'nodeapp', 'perlapp', 'phpapp', 'playapp',
            'prologapp', 'railsapp', 'rocketapp', 'springbootapp', 'swiftapp',
            'symfonyapp', 'tomcatapp', 'vibeapp', 'zigapp'
        ]:
        tester = Tester(host=nginx_host, port=nginx_port, name=app, endpoint=endpoint, method=method, assertions=assertions)
        try:
            assert tester.test()
            print("\x1b[32;20m"+f"{app} test passed for endpoint {endpoint} ({method})."+'\x1b[0m')
        except AssertionError:
            print("\x1b[31;1m"+f"{app} test failed for endpoint {endpoint} ({method})."+"\x1b[0m")
        except KeyError:
            print("\x1b[31;1m"+f"{app} test failed [KEY ERROR] for endpoint {endpoint} ({method})."+"\x1b[0m")


if __name__ == '__main__':
    run_tests()
