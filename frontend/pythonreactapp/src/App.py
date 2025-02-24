import_('BrowserRouter as Router', 'Route', 'Switch', _from='react-router-dom')
import_('UsersTable', _from='./components/UsersTable')

def App():
    return (
        div(
            Router(
                div(
                    Switch(
                        Route(path="", exact=True, component=UsersTable),
                        Route(path="users", component=UsersTable)
                    ),
                    className='container'
                )
            )
        )
    )

