const NGINX_HOST = process.env.GATSBY_APP_NGINX_HOST;
const NGINX_PORT = process.env.GATSBY_APP_NGINX_PORT;

const FLASK_APP = `http://${NGINX_HOST}:${NGINX_PORT}/flaskapp`;
const USER_API_BASE_URL = `${FLASK_APP}/users`;

module.exports = { USER_API_BASE_URL };
