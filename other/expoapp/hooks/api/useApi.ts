import { useColorScheme } from 'react-native';

import { useEffect, useState } from 'react';

import { Colors } from '@/constants/Colors';

import axios from 'axios';

import { apiUrl } from '@/constants/main';

export function useApi(
    //props: { light?: string; dark?: string }
) {
    let [users, setUsers] = useState([]);

    useEffect(() => {
        fetchUsers();
    }, []);

    const fetchUsers = async () => {
        try {
            const response = await axios.get(`${apiUrl}/users`, {headers:{"ngrok-skip-browser-warning": 'skip-browser-warning'}, mode: 'no-cors'});
            setUsers(response.data);
        } catch (error) {
            console.error("Error fetching users:", error);
        }
    }

    const createUser = async (email) => {
        try {
            const response = await axios.post(`${apiUrl}/users`, { email }, { headers: { "Content-Type": "application/json" } });
            setUsers([...users, response.data]);
        } catch (err) {
            console.error("Error creating user:", err);
        }
    };

    const updateUser = async (userId, email) => {
        try {
            await axios.put(`${apiUrl}/users/${userId}`, { email });
            setUsers(users.map((user) => (user.id === userId ? { ...user, email } : user)));
        } catch (err) {
            console.error("Error updating user:", err);
        }
    };

    const deleteUser = async (userId) => {
        try {
            await axios.delete(`${apiUrl}/users/${userId}`);
            setUsers(users.filter((user) => user.id !== userId));
        } catch (err) {
            console.error("Error deleting user:", err);
        }
    };

    return { users, fetchUsers, createUser, updateUser, deleteUser };
};
