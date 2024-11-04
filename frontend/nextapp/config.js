const dev = process.env.NODE_ENV !== 'production'

//module.exports = {
const exportable = {
  assetPrefix: '/nextapp',
  rewrites() {
    return [
      { source: '/docs/_next/:path*', destination: '/_next/:path*' }
    ]
  },
  output: 'export'
}

export default exportable;

export const assetPrefix = dev ? '/nextapp' : '/nextapp'
const NGINX_HOST = process.env.NGINX_HOST;
const NGINX_PORT = process.env.NGINX_PORT;

export const USER_API_BASE_URL = `http://${NGINX_HOST}:${NGINX_PORT}/flaskapp`;
