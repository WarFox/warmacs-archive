my_name = "Deepu"
my_const = 'new Const';

def hello_world():
    return "Hello world"

def hello_world_2():
    return "hewllo world"

def fibonacci(n):
    if n <= 1:
        return 1
    else:
        return fibonacci(n-1) + fibonacci(n-2)

class MyNewClass:
    def __init__(self):
        self.name = "Name"


def new_function():
    my_class= MyNewClass()
