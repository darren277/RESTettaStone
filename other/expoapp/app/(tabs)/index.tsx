import { Image, StyleSheet, TextInput, Button, Platform } from 'react-native';

import React, { useState, useEffect } from 'react';

import { HelloWave } from '@/components/HelloWave';
import ParallaxScrollView from '@/components/ParallaxScrollView';
import { ThemedText } from '@/components/ThemedText';
import { ThemedView } from '@/components/ThemedView';

import { useApi } from '@/hooks/api/useApi';

export default function HomeScreen() {
    const { users, createUser, updateUser, deleteUser } = useApi();

    const [newEmail, setNewEmail] = useState('');
    const [editingUser, setEditingUser] = useState(null);
    const [editingEmail, setEditingEmail] = useState('');

    const handleCreate = () => {
        if (!newEmail) return alert('Email is required');
        createUser(newEmail);
        setNewEmail('');
    };

    const handleUpdate = (userId) => {
        if (!editingEmail) return alert('Email is required');
        updateUser(userId, editingEmail);
        setEditingUser(null);
        setEditingEmail('');
    };

    if (!users) {
        return <ThemedText>Loading...</ThemedText>;
    };

    return (
        <ParallaxScrollView
            headerBackgroundColor={{ light: '#A1CEDC', dark: '#1D3D47' }}
            headerImage={
                <Image
                    source={require('@/assets/images/partial-react-logo.png')}
                    style={styles.reactLogo}
                />
            }>
            <ThemedView style={styles.titleContainer}>
                <ThemedText type="title">Welcome!</ThemedText>
                <HelloWave />
            </ThemedView>
            <ThemedView style={styles.container}>
                <ThemedText type="title">Users</ThemedText>
                {users.map((user) => (
                    <ThemedView key={user.id} style={styles.userRow}>
                        {editingUser === user.id ? (
                            <TextInput
                                style={styles.input}
                                defaultValue={user.email}
                                onChangeText={setEditingEmail}
                            />
                        ) : (
                            <ThemedText>{user.email}</ThemedText>
                        )}
                        {editingUser === user.id ? (
                            <Button title="Save" onPress={() => handleUpdate(user.id)} />
                        ) : (
                            <>
                                <Button title="Edit" onPress={() => setEditingUser(user.id)} />
                                <Button title="Delete" onPress={() => deleteUser(user.id)} />
                            </>
                        )}
                    </ThemedView>
                ))}
                <ThemedText type="subtitle">Add New User</ThemedText>
                <TextInput
                    style={styles.input}
                    placeholder="Enter email"
                    value={newEmail}
                    onChangeText={setNewEmail}
                />
                <Button title="Add User" onPress={handleCreate} />
            </ThemedView>
            <ThemedView style={styles.stepContainer}>
                <ThemedText type="subtitle">Users</ThemedText>{users.map((user) => {return (<ThemedText key={user.id}>{user.email}</ThemedText>)})}
                <ThemedText type="subtitle">Step 1: Try it</ThemedText>
                <ThemedText>
                    Edit <ThemedText type="defaultSemiBold">app/(tabs)/index.tsx</ThemedText> to see changes.
                    Press{' '}
                    <ThemedText type="defaultSemiBold">
                    {Platform.select({ ios: 'cmd + d', android: 'cmd + m' })}
                </ThemedText>{' '}
                    to open developer tools.
                </ThemedText>
            </ThemedView>
            <ThemedView style={styles.stepContainer}>
                <ThemedText type="subtitle">Step 2: Explore</ThemedText>
                <ThemedText>
                    Tap the Explore tab to learn more about what's included in this starter app.
                </ThemedText>
            </ThemedView>
            <ThemedView style={styles.stepContainer}>
                <ThemedText type="subtitle">Step 3: Get a fresh start</ThemedText>
                <ThemedText>
                    When you're ready, run{' '}
                    <ThemedText type="defaultSemiBold">npm run reset-project</ThemedText> to get a fresh{' '}
                    <ThemedText type="defaultSemiBold">app</ThemedText> directory. This will move the current{' '}
                    <ThemedText type="defaultSemiBold">app</ThemedText> to{' '}
                    <ThemedText type="defaultSemiBold">app-example</ThemedText>.
                </ThemedText>
            </ThemedView>
        </ParallaxScrollView>
    );
}

const styles = StyleSheet.create({
    container: {
        padding: 16,
        gap: 12,
    },
    userRow: {
        flexDirection: 'row',
        alignItems: 'center',
        justifyContent: 'space-between',
        marginBottom: 8,
    },
    input: {
        borderWidth: 1,
        borderColor: '#ccc',
        borderRadius: 4,
        padding: 8,
        flex: 1,
    },
    titleContainer: {
        flexDirection: 'row',
        alignItems: 'center',
        gap: 8,
    },
    stepContainer: {
        gap: 8,
        marginBottom: 8,
    },
    reactLogo: {
        height: 178,
        width: 290,
        bottom: 0,
        left: 0,
        position: 'absolute',
    }
});
