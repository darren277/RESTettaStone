const axios = require('axios');
const path = require('path');

const { USER_API_BASE_URL } = require('./constants');

exports.sourceNodes = async ({ actions, createNodeId, createContentDigest }) => {
    const { createNode } = actions;

    const res = await axios.get(USER_API_BASE_URL);

    // Create nodes for each user
    res.data.forEach(user => {
        const nodeContent = JSON.stringify(user);
        const nodeMeta = {
            id: String(user.id), // Use Postgres `id` directly as the Node's `id`
            parent: null,
            children: [],
            internal: {
                type: 'User',
                mediaType: 'application/json',
                content: nodeContent,
                contentDigest: createContentDigest(user),
            },
        };

        const node = Object.assign({}, user, nodeMeta);
        createNode(node);
    });
};

exports.createSchemaCustomization = ({ actions }) => {
    const { createTypes } = actions;
    const typeDefs = `
        type User implements Node {
            id: ID!
            email: String!
        }
    `;
    createTypes(typeDefs);
};

// Create pages dynamically for each user node
exports.createPages = async ({ graphql, actions }) => {
    const { createPage } = actions;
    const userTemplate = path.resolve(`./src/templates/user-page.js`);

    const result = await graphql(`
        {
            allUser {
                edges {
                    node {
                        id
                        email
                    }
                }
            }
        }
    `);

    if (result.errors) {
        console.error(result.errors);
        throw new Error("Failed to query users for creating pages");
    }

    // Create a page for each user node
    result.data.allUser.edges.forEach(({ node }) => {
        createPage({
            path: `/gatsbyapp/user/${node.id}`, // Path for each user page
            component: userTemplate, // Template component to use
            context: {
                id: node.id, // Pass user ID as context to the template
            },
        });
    });
};
