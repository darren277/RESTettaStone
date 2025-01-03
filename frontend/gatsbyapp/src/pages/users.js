import React, { useState } from 'react';
import { graphql } from 'gatsby';
import axios from 'axios';

import { USER_API_BASE_URL } from '../../constants';

const UserPage = ({ data }) => {
    const [users, setUsers] = useState(data.allUser.edges.map(({ node }) => node));
    const [newUser, setNewUser] = useState('');
    const [editingUser, setEditingUser] = useState(null);

    const handleCreate = async () => {
        if (!newUser) {
            alert('Email is required');
            return;
        }

        try {
            const res = await axios.post(USER_API_BASE_URL, { email: newUser });
            setUsers([...users, res.data]);
            setNewUser('');
        } catch (error) {
            console.error('Error creating user:', error);
        }
    };

    const handleUpdate = async (id, email) => {
        try {
            await axios.put(`${USER_API_BASE_URL}/${id}`, { email });
            setUsers(users.map(user => (user.id === id ? { ...user, email } : user)));
            setEditingUser(null);
        } catch (error) {
            console.error('Error updating user:', error);
        }
    };

    const handleDelete = async id => {
        try {
            await axios.delete(`${USER_API_BASE_URL}/${id}`);
            setUsers(users.filter(user => user.id !== id));
        } catch (error) {
            console.error('Error deleting user:', error);
        }
    };

    return (
        <div>
            <h1>Users</h1>
            <div>
                <input type="email" value={newUser} onChange={e => setNewUser(e.target.value)} placeholder="New user email" />
                <button onClick={handleCreate}>Add User</button>
            </div>

            {users.map(user => (
                <div key={user.id}>
                    {editingUser === user.id ? (
                        <div>
                            <input type="email" defaultValue={user.email} onBlur={e => handleUpdate(user.id, e.target.value)} />
                            <button onClick={() => setEditingUser(null)}>Done</button>
                        </div>
                    ) : (
                        <h3>
                            {user.email}
                            <button onClick={() => setEditingUser(user.id)}>Edit</button>
                            <button onClick={() => handleDelete(user.id)}>Delete</button>
                        </h3>
                    )}
                    <hr />
                </div>
            ))}
        </div>
    );
};

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
