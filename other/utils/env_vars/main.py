""""""

app_name_list = [
    'ReactFiber',
    'Vue',
    'Angular',
    'Next',
    'Gatsby',
    'React',

    'Firebase'
    
    'Crow',
    'Rails',
    'Rocket',
    'AspNet',
    'Bun',
    'PHP',
    'SpringBoot',
    'Flask',
    'Go',
    'Cobol',
    'Prolog',
    'Perl',
    'Lisp',
    'Haskell',
    'Fortran',
    'Laravel',
    'Node',
    'Actix',
    'Spock',
    'Pascal',
    'Zig',
    'Vibe',
    'FSharp',
    'Clojure',
    'Swift',
    'FatFree',
    'Play',
    'Symfony',
    'Lua',
    'Django',
    'Tomcat'
]


app_env_vars_list = []

start_ip = '172.18.0.20'
start_port = 3001

for app_name in app_name_list:
    app_env_vars_list.append(f'{app_name.upper()}APP_IP={start_ip}')
    app_env_vars_list.append(f'{app_name.upper()}APP_PORT={start_port}')
    start_ip = '.'.join(start_ip.split('.')[:-1]) + '.' + str(int(start_ip.split('.')[-1]) + 1)
    start_port += 1

print('\n'.join(app_env_vars_list))

