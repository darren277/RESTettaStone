require("dotenv").config({
  //path: `.env.${process.env.NODE_ENV}`,
  path: '.env'
})

module.exports = {
  siteMetadata: {
    title: 'GatsbyApp',
  },
  pathPrefix: '/gatsbyapp',
  plugins: [
    'gatsby-plugin-react-helmet',
    'gatsby-plugin-catch-links',
    {
      resolve: 'gatsby-source-filesystem',
      options: {
        path: `${__dirname}/src/pages`,
        name: 'pages',
      },
    },
    'gatsby-transformer-remark'
  ],
}
