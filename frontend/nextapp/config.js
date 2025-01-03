const dev = process.env.NODE_ENV !== 'production'

//export const assetPrefix = dev ? '/nextapp' : '/nextapp'
const NGINX_HOST = process.env.NGINX_HOST;
const NGINX_PORT = process.env.NGINX_PORT;

export const USER_API_BASE_URL = `http://${NGINX_HOST}:${NGINX_PORT}/flaskapp`;

const RUN_TIME_NGINX_HOST = process.env.NEXT_PUBLIC_NGINX_HOST;
const RUN_TIME_NGINX_PORT = process.env.NEXT_PUBLIC_NGINX_PORT;

export const RUN_TIME_USER_API_BASE_URL = `http://${RUN_TIME_NGINX_HOST}:${RUN_TIME_NGINX_PORT}/flaskapp`;

//module.exports = {
const exportable = {
    assetPrefix: '/nextapp', // Ensures static assets are served under `/nextapp`
    basePath: '/nextapp', // Ensures all routes are prefixed with `/nextapp`
    env: {
        NGINX_HOST: NGINX_HOST,
        NGINX_PORT: NGINX_PORT,
        USER_API_BASE_URL: USER_API_BASE_URL,
        RUN_TIME_NGINX_HOST: RUN_TIME_NGINX_HOST,
        RUN_TIME_NGINX_PORT: RUN_TIME_NGINX_PORT,
        RUN_TIME_USER_API_BASE_URL: RUN_TIME_USER_API_BASE_URL
    },
    async rewrites() {
        //return [];
        return [
            { source: '/nextapp/_next/:path*', destination: '/_next/:path*' },
            { source: '/nextapp/flaskapp/:path*', destination: '/flaskapp/:path*' }
        ];
    },
    //trailingSlash: false
}

export default exportable;
//module.exports = exportable;
