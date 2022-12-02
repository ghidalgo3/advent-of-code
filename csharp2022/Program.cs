// See https://aka.ms/new-console-template for more information
using System.IO;

var input = File.ReadLines("inputs/1");
var accum = 0;
var max = 0;
List<int> allElves = new();

foreach (var line in input)
{
    switch (line) {
        case "":
            if (accum > max)
            {
                max = accum;
            }
            allElves.Add(accum);
            accum = 0;
            break;

        default:
            accum += int.Parse(line);
            break;

    }
}

allElves.Sort();
allElves.Reverse();

Console.WriteLine(allElves.Take(3).Sum());