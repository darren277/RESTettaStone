#from axios import axios
import_('axios', _from='axios')

NGINX_HOST = process.env.REACT_APP_NGINX_HOST
NGINX_PORT = process.env.REACT_APP_NGINX_PORT

FLASK_APP = f'http://{NGINX_HOST}:{NGINX_PORT}/flaskapp'

USER_API_BASE_URL = f'{FLASK_APP}/users'


class UserService:
    @staticmethod
    async def getUsers(): return axios.get(USER_API_BASE_URL)

    @staticmethod
    async def createUser(user): return axios.post(USER_API_BASE_URL, user, {headers: { 'Content-Type': 'application/json' }})

    @staticmethod
    async def getUserById(userId): return axios.get(USER_API_BASE_URL + '/' + userId)

    @staticmethod
    async def updateUser(userId, user): return axios.put(USER_API_BASE_URL + '/' + userId, user, {headers: { 'Content-Type': 'application/json' }})

    @staticmethod
    async def deleteUser(userId): return axios.delete(USER_API_BASE_URL + '/' + userId)


export_default(UserService)
