typedef int fi;

fi f(fi x,fi y)
{
    int a = 0;
    switch(x)
    {
        case 1:
            a = a+1;
            a = a+0;
            break;
            a = a+1;
        case 2:
            if(y>0)
            {
                a=a+20;
                switch(y)
                {
                    case 0:
                        a=a; // dead!
                    case 1:
                        a=a+100;
                    case 2:
                        a=a+50;
                        break;
                    default:
                        a=a+1000;
                }
            }
            else
            {
                for(int i=0;i<3;i++)
                //          ^^^
                //          Dead Else because of if(i>=2) break;
                {
                    if(i>=2) break;
                    a++;
                }
                a++;
                break;
            }
            a = a + 3;
        case 3:
            a = a + 4;
            break;
        // No default here
    }
    a++;
    return a;
}
