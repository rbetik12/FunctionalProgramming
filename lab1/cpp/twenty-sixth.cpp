#include <iostream>

int symma(int n)
{
    int sum = 0;
    while (n != 0)
    {
        sum = sum + (n % 10);
        n = n / 10;
    }
    return sum;
}

int main()
{
    int maxIndex = -1;
    int x;
    for (int i = 1; i < 1000; i++)
    {
        x = i;
        std::cout << x << std::endl;
        int k = 0;
        while (x != 0)
        {
            x = x - symma(x);
            k++;
        }

        if (k > maxIndex)
        {
            maxIndex = i;
        }
    }
    std::cout << maxIndex << std::endl;
}