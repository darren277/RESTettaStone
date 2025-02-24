from react import useState
import_('useUsers', _from='../hooks/useUsers')

def UsersTable():
    (users, addUser, editUser, deleteUser) = useUsers()
    [newUser, setNewUser] = useState({'firstName': '', 'lastName': '', 'email': ''})
    [editingUserId, setEditingUserId] = useState(None)
    [editingData, setEditingData] = useState({'firstName': '', 'lastName': '', 'email': ''})

    def handleAddInputChange(e):
        name, value = e.target
        setNewUser(lambda prev: ({ ...: prev, [name]: value }))

    def handleAddUser(e):
        e.preventDefault()
        addUser(newUser)
        setNewUser({'firstName': '', 'lastName': '', 'email': ''})

    def handleEditClick(user):
        setEditingUserId(user.id)
        setEditingData({'firstName': user.firstName, 'lastName': user.lastName, 'email': user.email})

    def handleEditInputChange(e):
        (name, value) = e.target
        setEditingData(lambda prev: ({ **prev, [name]: value }))

    def handleSaveEditedUser(e):
        e.preventDefault()
        editUser(editingUserId, editingData)
        setEditingUserId(None)

    def handleCancelEdit():
        setEditingUserId(None)

    return (
        div(
            h2('Users List', className='text-center'),
            table(
                thead(
                    tr(
                        th('First Name'),
                        th('Last Name'),
                        th('Email'),
                        th('Actions')
                    )
                ),
                tbody(
                    [tr(
                        td(
                            input_(type='text', name='firstName', value=editingData.firstName, onChange=handleEditInputChange, required=True)
                        ),
                        td(
                            input_(type='text', name='lastName', value=editingData.lastName, onChange=handleEditInputChange, required=True)
                        ),
                        td(
                            input_(type='email', name='email', value=editingData.email, onChange=handleEditInputChange, required=True)
                        ),
                        td(
                            button('Save', className='btn btn-success', onClick=handleSaveEditedUser),
                            button('Cancel', className='btn btn-secondary', onClick=handleCancelEdit, style={'marginLeft': '10px'})
                        ),
                        key=user.id
                    ) if editingUserId == user.id else tr(
                        td(user.firstName),
                        td(user.lastName),
                        td(user.email),
                        td(
                            button('Edit', className='btn btn-info', onClick=lambda: handleEditClick(user)),
                            button('Delete', className='btn btn-danger', onClick=lambda: deleteUser(user.id), style={'marginLeft': '10px'})
                        ),
                        key=user.id
                    ) for user in users]
                ),
                className='table table-striped table-bordered',
            ),
            div(
                h4('Add New User'),
                form(
                    div(
                        input_(type='text', name='firstName', placeholder='First Name', value=newUser.firstName, onChange=handleAddInputChange, required=True),
                        input_(type='text', name='lastName', placeholder='Last Name', value=newUser.lastName, onChange=handleAddInputChange, required=True),
                        input_(type='email', name='email', placeholder='Email', value=newUser.email, onChange=handleAddInputChange, required=True),
                        style={'display': 'flex', 'gap': '10px', 'marginBottom': '10px'}
                    ),
                    button('Add User', type='submit', className='btn btn-primary', onClick=handleAddUser)
                ),
                style={'marginTop': '20px'}
            )
        )
    )

export_default(UsersTable)
