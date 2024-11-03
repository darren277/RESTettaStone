import React from 'react';
import { graphql } from 'gatsby';

const UserPage = ({ data }) => (
  <div>
    <h1>Users</h1>
    {data.allUser.edges.map(({ node }) => (
      <div key={node.id}>
        <h3>{node.email}</h3>
        <hr />
      </div>
    ))}
  </div>
);

export const query = graphql`
  query {
    allUser {
      edges {
        node {
          id
          email
        }
      }
    }
  }
`;

export default UserPage;
