// See https://aka.ms/new-console-template for more information
using System.Text.RegularExpressions;
string? GetMatchingGroupName(Match match, Regex regex)
{
    foreach (var groupName in regex.GetGroupNames().Skip(1))
    {
        if (match.Groups[groupName].Success)
        {
            return groupName;
        }
    }

    return null;
}

int GetCalibrationValue(string text)
{
    Regex numberRegex = new("(?<one>one|1)|(?<two>two|2)|(?<three>three|3)|(?<four>four|4)|(?<five>five|5)|(?<six>six|6)|(?<seven>seven|7)|(?<eight>eight|8)|(?<nine>nine|9)");
    List<int> numbers = new();
    int startAt = 0;
    while (startAt < text.Length)
    {
        var match = numberRegex.Match(text, startAt);
        if (match.Success)
        {
            var groupName = GetMatchingGroupName(match, numberRegex);
            // Console.WriteLine(groupName);
            numbers.Add(groupName switch
            {
                "one" => 1,
                "two" => 2,
                "three" => 3,
                "four" => 4,
                "five" => 5,
                "six" => 6,
                "seven" => 7,
                "eight" => 8,
                "nine" => 9
            });
            startAt = match.Index + 1;
        }
        else
        {
            break;
        }
    }
    // foreach (Match match in matches)
    // {
    //     var groupName = match.Groups[0].Value;
    // }

    if (numbers.Count == 1)
    {
        return numbers[0] * 10 + numbers[0];
    }
    else
    {
        return numbers[0] * 10 + numbers[numbers.Count - 1];
    }
}
// day 1
Console.Write("Day 1: ");
Console.WriteLine(File.ReadAllLines("input/1").Select(GetCalibrationValue).Sum());

Console.Write("Day 2-1: ");
var game = new Game(12, 13, 14);
var day2Input = File.ReadAllLines("input/2");
Console.WriteLine(day2Input.Select((l, idx) => (game.IsPossible(l), idx + 1)).Where((p, i) => p.Item1).Sum(g => g.Item2));
Console.Write("Day 2-2: ");
Console.WriteLine(day2Input.Select(l => game.Minimum(l)).Sum(g => g.Power));