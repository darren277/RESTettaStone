import { useColorScheme } from 'react-native';

import { useEffect, useState } from 'react';

import { Colors } from '@/constants/Colors';

import axios from 'axios';

import { apiUrl } from '@/constants/main';

export function useApi(
    //props: { light?: string; dark?: string }
) {
    // apiUrl
    let [users, setUsers] = useState([]);

    useEffect(() => {
        axios.get(`${apiUrl}/users`,
        {headers:{"ngrok-skip-browser-warning": 'skip-browser-warning'}, mode: 'no-cors'}).then((response) => {
            setUsers(response.data);
        });
    }, []);

    return [users, setUsers];
};
