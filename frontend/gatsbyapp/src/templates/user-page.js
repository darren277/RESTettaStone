import React from 'react';
import { graphql } from 'gatsby';

const UserPageTemplate = ({ data }) => {
    const { user } = data;

    return (
        <div>
            <h1>User Detail</h1>
            <p>ID: {user.id}</p>
            <p>Email: {user.email}</p>
        </div>
    );
};

export default UserPageTemplate;

export const query = graphql`
    query($id: String!) {
        user(id: { eq: $id }) {
            id
            email
        }
    }
`;
