import axios from 'axios';

const NGINX_HOST = process.env.REACT_APP_NGINX_HOST;
const NGINX_PORT = process.env.REACT_APP_NGINX_PORT;

const FLASK_APP = `http://${NGINX_HOST}:${NGINX_PORT}/flaskapp`;

const USER_API_BASE_URL = `${FLASK_APP}/users`;


class UserService {
    getUsers(){return axios.get(USER_API_BASE_URL);}
    createUser(user){return axios.post(USER_API_BASE_URL, user);}
    getUserById(userId){return axios.get(USER_API_BASE_URL + '/' + userId);}
    updateUser(user, userId){return axios.put(USER_API_BASE_URL + '/' + userId, user);}
    deleteUser(userId){return axios.delete(USER_API_BASE_URL + '/' + userId);}
}

export default new UserService()
