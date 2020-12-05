using System;
using System.Threading.Tasks;
using System.IO;
using Seatid;

namespace advent5stream
{
    class Program
    {
        static async Task Main(string[] args)
        {
            var pipeline = new SeatidStream(Console.OpenStandardInput(), Console.OpenStandardOutput());
            await pipeline.Start();

/*
            using (var inStream = new MemoryStream()) 
            {
                using (var outStream = new MemoryStream())
                {
                    using (var writer = new StreamWriter(inStream))
                    {
                        //writer.Write("FBFBBFFRLR\n");
                        var input = File.OpenRead("input5.txt");
                        input.CopyTo(inStream);
                        input.Flush();
                        writer.Flush();
                        inStream.Position = 0;

                        var pipe = new SeatidStream(inStream, outStream);
                        var pipeTask = pipe.Start();
                        await pipeTask;
                        outStream.Seek(0, SeekOrigin.Begin);
                        var sr = new StreamReader(outStream);
                        var allText = sr.ReadToEnd();
//                        if ("357" != allText.TrimEnd(Environment.NewLine.ToCharArray())) {
//                            throw new Exception("oops");
//                        }
                    }
                }
            }
            */
       }
    }
}
