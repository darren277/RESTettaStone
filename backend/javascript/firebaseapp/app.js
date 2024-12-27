// import { getAuth, signInAnonymously } from "firebase/auth";
const express = require('express');
const cors = require('cors');
const bodyParser = require('body-parser');
const { initializeApp } = require('firebase/app');
const { getFirestore, collection, query, where, doc, addDoc, getDoc, getDocs, updateDoc, deleteDoc } = require('firebase/firestore');

const dotenv = require('dotenv');

dotenv.config();

const firebaseConfig = {
    apiKey: process.env.FIREBASE_API_KEY,
    authDomain: process.env.FIREBASE_AUTH_DOMAIN,
    projectId: process.env.FIREBASE_PROJECT_ID,
    storageBucket: process.env.FIREBASE_STORAGE_BUCKET,
    messagingSenderId: process.env.FIREBASE_MESSAGING_SENDER_ID,
    appId: process.env.FIREBASE_APP_ID,
    measurementId: process.env.FIREBASE_MEASUREMENT_ID
};

const firebase = initializeApp(firebaseConfig);

// const googleLogin = () => {const provider = new firebase.auth.GoogleAuthProvider(); firebase.auth().signInWithPopup(provider).then((result) => {const user = result.user; document.write(`Hello ${user.displayName}`); console.log(user);});};
// const auth = getAuth();
// signInAnonymously(auth).then(() => {const user = result.user; document.write(`Hello ${user.displayName}`); console.log(user);}).catch((error) => {const errorCode = error.code; const errorMessage = error.message;});

const firestore = getFirestore(firebase);


class User {
    constructor(id, email ) {
            this.id = id;
            this.email = email;
    }
}

// const query = users.where('id', '==', 1);
const addUser = async (req, res, next) => {
    try {
        const data = req.body;
        const users_collection = await collection(firestore, 'users');
        const result = await addDoc(users_collection, data);
        if (!result) {
            res.status(500).send('Record not saved');
        }
        res.send({'Success': 'Record saved successfuly', 'id': result.id, data});
    } catch (error) {
        res.status(400).send(error.message);
    }
}

const getAllUsers = async (req, res, next) => {
    try {
        const users = await collection(firestore, 'users');
        const q = query(users);
        const data = await getDocs(q);
        const usersArray = [];
        if(data.empty) {
            res.status(404).send('No user record found');
        } else {
            data.forEach(doc => {
                const user = new User(doc.id, doc.data().email);
                usersArray.push(user);
            });
            res.send(usersArray);
        }
    } catch (error) {
        res.status(400).send(error.message);
    }
}

const getUser = async (req, res, next) => {
    try {
        const id = req.params.id;
        const user_collection = collection(firestore, 'users');
        const user = doc(user_collection, id);
        const data = await getDoc(user);

        if (!data.exists) {
            res.status(404).send('User with the given ID not found');
        } else {
            res.send(data.data());
        }
    } catch (error) {
        res.status(400).send(error.message);
    }
}

const updateUser = async (req, res, next) => {
    try {
        const id = req.params.id;
        const data = req.body;
        const user_collection = await collection(firestore, 'users');
        const user = await doc(user_collection, id);

        const result = await updateDoc(user, data);
        console.log('result', result);
        res.send('User record updated successfuly');
    } catch (error) {
        if (error.code === 'not-found') {
            // Response (400): 5 NOT_FOUND: No document to update: projects/fir-app-3d1ca/databases/(default)/documents/users/11FdZqjmzRRgGJynQ1Oj
            res.status(404).send('User with the given ID not found');
        }
        res.status(400).send(error.message);
    }
}

const deleteUser = async (req, res, next) => {
    try {
        const id = req.params.id;

        if (!id) {
            res.status(400).send('User ID is required');
        }

        if (id === 'DELETE_ALL') {
            const users = await collection(firestore, 'users');
            const q = query(users);
            const data = await getDocs(q);
            data.forEach(async doc => {
                await deleteDoc(doc.ref);
            });
            res.send('All records deleted successfuly');
        } else{
            console.log('req', req.params.id);
            const user_collection = await collection(firestore, 'users');
            const user = await doc(user_collection, id);
            const result = await getDoc(user);
            if (!result.exists) {
                res.status(404).send('User with the given ID not found');
            } else {
                await deleteDoc(user);
                res.send('Record deleted successfuly');
            }
        }
    } catch (error) {
        res.status(400).send(error.message);
    }
}


const router = express.Router();

router.post('/users', addUser);
router.get('/users', getAllUsers);
router.get('/users/:id', getUser);
router.put('/users/:id', updateUser);
router.delete('/users/:id', deleteUser);


const app = express();

app.use(express.json());
app.use(cors());
app.use(bodyParser.json());

app.use('/', router);

const port = process.env.PORT || 3000;


app.listen(port, () => console.log('App is listening on url http://localhost:' + port));
