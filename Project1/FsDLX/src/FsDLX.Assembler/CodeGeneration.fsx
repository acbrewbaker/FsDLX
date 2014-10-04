open System
open System.IO
open System.Text.RegularExpressions

        let matchText input = 
            let matches = Regex(@"(?<=\.(text).*)(?<text>\d+)").Matches(input)
            ([for m in matches -> m.Groups.["text"].Value], matches.Count > 0)
