#include <iostream>
#include <unordered_map>

int main()
{
    int nMax = 0;
    int index = 0;
    std::unordered_map<int, int> firstPos;

    for (int i = 1; i < 1000; i++)
    {
        int n = i;
        int position = 0;
        std::string period = "";
        int rem = 1;
        firstPos.clear();

        while (firstPos.find(rem) == firstPos.end())
        {
            firstPos[rem] = position;
            period += std::to_string(rem / n);
            rem = (rem % n) * 10;
            position += 1;
        }

        if (period.size() > nMax)
        {
            nMax = period.size();
            index = i;
        }
    }

    std::cout << nMax << std::endl;
    std::cout << index << std::endl;
}