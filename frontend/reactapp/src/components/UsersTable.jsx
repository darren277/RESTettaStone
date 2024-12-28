import React, { useState } from 'react';
import { useUsers } from '../hooks/useUsers';

const UsersTable = () => {
    const { users, addUser, editUser, deleteUser } = useUsers();
    const [newUser, setNewUser] = useState({ firstName: '', lastName: '', email: '' });
    const [editingUserId, setEditingUserId] = useState(null);
    const [editingData, setEditingData] = useState({ firstName: '', lastName: '', email: '' });

    const handleAddInputChange = (e) => {
        const { name, value } = e.target;
        setNewUser((prev) => ({ ...prev, [name]: value }));
    };

    const handleAddUser = (e) => {
        e.preventDefault();
        addUser(newUser);
        setNewUser({firstName: '', lastName: '', email: ''});
    };

    const handleEditClick = (user) => {
        setEditingUserId(user.id);
        setEditingData({firstName: user.firstName, lastName: user.lastName, email: user.email});
    };

    const handleEditInputChange = (e) => {
        const { name, value } = e.target;
        setEditingData((prev) => ({ ...prev, [name]: value }));
    };

    const handleSaveEditedUser = (e) => {
        e.preventDefault();
        editUser(editingUserId, editingData);
        setEditingUserId(null);
    };

    const handleCancelEdit = () => {
        setEditingUserId(null);
    };

    return (
        <div>
            <h2 className="text-center">Users List</h2>
            <table className="table table-striped table-bordered">
                <thead>
                    <tr>
                        <th>First Name</th>
                        <th>Last Name</th>
                        <th>Email</th>
                        <th>Actions</th>
                    </tr>
                </thead>
                <tbody>
                    {users.map((user) => {
                        if (editingUserId === user.id) {
                            return (
                                <tr key={user.id}>
                                    <td>
                                        <input type="text" name="firstName" value={editingData.firstName} onChange={handleEditInputChange} required />
                                    </td>
                                    <td>
                                        <input type="text" name="lastName" value={editingData.lastName} onChange={handleEditInputChange} required />
                                    </td>
                                    <td>
                                        <input type="email" name="email" value={editingData.email} onChange={handleEditInputChange} required />
                                    </td>
                                    <td>
                                        <button className="btn btn-success" onClick={handleSaveEditedUser} >Save</button>
                                        <button className="btn btn-secondary" onClick={handleCancelEdit} style={{ marginLeft: '10px' }} >Cancel</button>
                                    </td>
                                </tr>
                            );
                        }

                        return (
                            <tr key={user.id}>
                                <td>{user.firstName}</td>
                                <td>{user.lastName}</td>
                                <td>{user.email}</td>
                                <td>
                                    <button className="btn btn-info" onClick={() => handleEditClick(user)}>Edit</button>
                                    <button className="btn btn-danger" onClick={() => deleteUser(user.id)} style={{ marginLeft: '10px' }} >Delete</button>
                                </td>
                            </tr>
                        );
                    })}
                </tbody>
            </table>

            <div style={{ marginTop: '20px' }}>
                <h4>Add New User</h4>
                <form onSubmit={handleAddUser}>
                    <div style={{ display: 'flex', gap: '10px', marginBottom: '10px' }}>
                        <input type="text" name="firstName" placeholder="First Name" value={newUser.firstName} onChange={handleAddInputChange} required />
                        <input type="text" name="lastName" placeholder="Last Name" value={newUser.lastName} onChange={handleAddInputChange} required />
                        <input type="email" name="email" placeholder="Email" value={newUser.email} onChange={handleAddInputChange} required />
                    </div>
                    <button type="submit" className="btn btn-primary">Add User</button>
                </form>
            </div>
        </div>
    );
};

export default UsersTable;
