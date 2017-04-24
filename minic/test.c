int f(){
    return 0;
}

int g(){
    return 0;
}

int h(){
    return 0;
}

int main () {
    int i;
    for( i = 0 ; i < 3 ; i++)
        g()+h();
    return f() + g() + h();
}

