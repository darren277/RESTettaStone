import React from 'react'
import PropTypes from 'prop-types'
import Helmet from 'react-helmet'

import { graphql } from 'gatsby'

import Header from '../components/header'
import Menu from '../components/menu'
import './index.css'

const Layout = ({ children, data }) => (
    <div>
        <Helmet
            title={data.site.siteMetadata.title}
            meta={[{name: 'description', content: 'The Gatsby App'}, { name: 'keywords', content: 'gatsby, react, tutorial' }]}
        />
        <Header siteTitle={data.site.siteMetadata.title} />
        <Menu />
        <div style={{margin: '0 auto', maxWidth: 960, padding: '0px 1.0875rem 1.45rem', paddingTop: 0}}>
            {children()}
        </div>
    </div>
)

Layout.propTypes = {
    children: PropTypes.func,
}

export default Layout

export const query = graphql`
    query SiteTitleQuery {
        site {
            siteMetadata {
                title
            }
        }
    }
`
