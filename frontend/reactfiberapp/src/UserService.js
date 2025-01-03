import axios from 'axios';

import ENV_VARS from './config.js';


const { REACTFIBER_APP_NGINX_HOST, REACTFIBER_APP_NGINX_PORT } = ENV_VARS;

const NGINX_HOST = REACTFIBER_APP_NGINX_HOST;
const NGINX_PORT = REACTFIBER_APP_NGINX_PORT;

const FLASK_APP = `http://${NGINX_HOST}:${NGINX_PORT}/flaskapp`;

const USER_API_BASE_URL = `${FLASK_APP}/users`;


class UserService {
    getUsers(){return axios.get(USER_API_BASE_URL);}
    createUser(user){return axios.post(USER_API_BASE_URL, user, {headers: { 'Content-Type': 'application/json' }});}
    getUserById(userId){return axios.get(USER_API_BASE_URL + '/' + userId);}
    updateUser(userId, user){return axios.put(USER_API_BASE_URL + '/' + userId, user, {headers: { 'Content-Type': 'application/json' }});}
    deleteUser(userId){return axios.delete(USER_API_BASE_URL + '/' + userId);}
}

export default new UserService()
