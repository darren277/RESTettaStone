from react import useState
from react import useEffect


class UserService:
    @staticmethod
    async def getUsers():
        return {'data': [{'id': 1, 'email': 'email1'}, {'id': 2, 'email': 'email2'}]}

    @staticmethod
    async def createUser(user):
        return {'data': {'id': 3, 'email': user['email']}}

    @staticmethod
    async def updateUser(id, updatedUser):
        return {'data': {'id': id, 'email': updatedUser['email']}}

    @staticmethod
    async def deleteUser(id):
        return None

    def then(self, func):
        return func(self)


def useUsers():
    [users, setUsers] = useState([])

    def getUsers():
        UserService.getUsers().then(lambda res: setUsers(res.data)).catch(lambda err: console.log(err))

    useEffect(lambda: getUsers(), [])

    def addUser(user):
        creatableUser = {email: user.email}
        UserService.createUser(creatableUser).then(lambda res: setUsers([*users, res.data]).catch(lambda err: console.log(err)))

    def editUser(id, updatedUser):
        updatableUser = {email: updatedUser.email}
        # NOTE: It's a whole lot simpler to use my custom `ternary` function here than a ternary literal.
        UserService.updateUser(id, updatableUser).then(lambda res: setUsers(users.map(lambda user: ternary(user.id == id, res.data, user))).catch(lambda err: console.log(err)))

    def deleteUser(id):
        UserService.deleteUser(id).then(lambda: setUsers(users.filter(lambda user: user.id != id)).catch(lambda err: console.log(err)))

    return {users, addUser, editUser, deleteUser}

export_default(useUsers)
