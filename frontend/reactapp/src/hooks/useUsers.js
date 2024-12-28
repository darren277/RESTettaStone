import { useState, useEffect } from 'react';
import UserService from '../services/UserService';

const useUsers = () => {
    const [users, setUsers] = useState([]);

    useEffect(() => {
        UserService.getUsers().then((res) => setUsers(res.data)).catch((err) => console.log(err));
    }, []);

    const addUser = (user) => {
        // filter out firstName and lastName as we do not save those yet...
        user = {email: user.email};
        UserService.createUser(user).then((res) => setUsers([...users, res.data])).catch((err) => console.log(err));
    };

    const editUser = (id, updatedUser) => {
        // filter out firstName and lastName as we do not save those yet...
        updatedUser = {email: updatedUser.email};
        UserService.updateUser(id, updatedUser).then((res) => {
            setUsers(users.map((user) => (user.id === id ? res.data : user)));
        }).catch((err) => console.log(err));
    };

    const deleteUser = (id) => {
        UserService.deleteUser(id).then(() => {
            setUsers(users.filter((user) => user.id !== id));
        }).catch((err) => console.log(err));
    };

    return { users, addUser, editUser, deleteUser };
};

export {useUsers};
