// See https://aka.ms/new-console-template for more information
int GetCalibrationValue(string text)
{
    List<int> numbers = new();
    foreach (char c in text)
    {
        if (c >= '0' && c <= '9')
        {
            numbers.Add(c - '0');
        }
    }
    if (numbers.Count == 1)
    {
        return numbers[0] * 10 + numbers[0];
    }
    else
    {
        return numbers[0] * 10 + numbers[numbers.Count - 1];
    }
}

Console.WriteLine(File.ReadAllLines("input/1").Select(GetCalibrationValue).Sum());