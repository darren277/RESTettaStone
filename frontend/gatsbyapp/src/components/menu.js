import React from 'react'
import Link from 'gatsby-link'

const Menu = () => (
    <div
        style={{
            background: '#f4f4f4',
            paddingTop: '10px',
        }}
    >
        <ul
            style={{
                listStyle: 'none',
                display: 'flex',
                justifyContent: 'space-evenly',
            }}
        >
            <li>
                <Link to="/">Home</Link>
            </li>
            <li>
                <Link to="/users">Users</Link>
            </li>
        </ul>
    </div>
)

export default Menu
