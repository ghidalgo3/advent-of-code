public record class Game(int Red, int Green, int Blue)
{
    public int Power => Red * Green * Blue;

    public Game Minimum(string observations)
    {
        var game = new Game(0, 0, 0);
        foreach (var (red, green, blue) in Parse(observations))
        {
            game = game with
            {
                Red = Math.Max(game.Red, red),
                Green = Math.Max(game.Green, green),
                Blue = Math.Max(game.Blue, blue)
            };
        }
        return game;
    }

    public bool IsPossible(string observations)
    {
        foreach (var (red, green, blue) in Parse(observations))
        {
            if (red > Red || green > Green || blue > Blue)
            {
                return false;
            }
        }
        return true;
    }

    private List<(int red, int green, int blue)> Parse(string game)
    {
        List<(int red, int green, int blue)> result = [];
        var games = game.Substring(game.IndexOf(":") + 2);
        foreach(var round in games.Split(";"))
        {
            var r = (red: 0, green: 0, blue: 0);
            foreach (var colorValue in round.Split(", "))
            {
                int value = int.Parse(colorValue.Trim().Split(" ")[0]);
                if (colorValue.Contains("red"))
                {
                    r.red = value;
                }
                else if (colorValue.Contains("green"))
                {
                    r.green = value;
                }
                else
                {
                    r.blue = value;
                }
            }
            result.Add(r);
        }
        return result;
    }
}
