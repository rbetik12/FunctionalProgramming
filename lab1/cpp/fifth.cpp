#include <iostream>

int main()
{
    for (int i = 100; i < 1000000000; i++)
    {
        bool result = true;
        for (int j = 1; j <= 20; j++)
        {
            if (i % j != 0)
            {
                result = false;
                break;
            }
        }

        if (result)
        {
            std::cout << "Result: " << i << std::endl;
            break;
        }
    }
    return 0;
}
