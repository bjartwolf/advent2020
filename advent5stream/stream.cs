using System;
using System.Buffers;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.IO.Pipelines;
using System.Threading.Tasks;

namespace Seatid
{
    public class SeatidStream
    {
        private string _ignorepattern;
        private Stream _outStream;
        private Stream _inStream;
        private PipeReader _reader;
        private PipeWriter _writer;

        public SeatidStream(Stream inputStream, Stream outputStream, string ignorepattern = "DSFSADFDSAFASDFSDA")
        {
            _ignorepattern = ignorepattern;
            var readerOpts = new StreamPipeReaderOptions(); 
            _reader = PipeReader.Create(inputStream, readerOpts);
            var writerOptions = new StreamPipeWriterOptions(leaveOpen: true);
            _writer = PipeWriter.Create(outputStream, writerOptions);
            _outStream = outputStream;
            _inStream = inputStream;
        }

        public Task Start() {
            return ProcessLines();
        }

        private async Task ProcessLines()
        {
            while (true)
            {
                ReadResult result = await _reader.ReadAsync();
                ReadOnlySequence<byte> buffer = result.Buffer;

                while (TryReadLine(ref buffer, out ReadOnlySequence<byte> line))
                {
                    // Process the line.
                    ProcessLine(line);
                    await _writer.FlushAsync();
                }

                // Tell the PipeReader how much of the buffer has been consumed.
                _reader.AdvanceTo(buffer.Start, buffer.End);

                // Stop reading if there's no more data coming.
                if (result.IsCompleted)
                {
                    break;
                }
            }
            await _reader.CompleteAsync();
            await _writer.CompleteAsync();
         }
        private bool TryReadLine(ref ReadOnlySequence<byte> buffer, out ReadOnlySequence<byte> line)
        {
            // Look for a EOL in the buffer.
            //SequencePosition? position = buffer.PositionOf((byte)'\n');
            SequencePosition? position = buffer.PositionOf((byte)10);

            if (position == null)
            {
                line = default;
                return false;
            } 

            // Skip the line + the \n.
            line = buffer.Slice(0, position.Value);
            buffer = buffer.Slice(buffer.GetPosition(1, position.Value));
            return true;
        }

        private uint GetSeatid(ReadOnlySequence<byte> buffer) {
            uint sum = 0;
            foreach (var segment in buffer) {
                var i = 0;
                var length = segment.Length;
                foreach (var element in segment.Span) {
                    uint a = 0b_0000_0100;
                    sum += ((element & a) ^a) >> 2 << length - i -1;
                    i++;
                }
            }
            return sum;
        }

        private void ProcessLine(in ReadOnlySequence<byte> buffer)
        {
            // should be 10 characters. Should really
            // never end up with multiple segments...
            var seatId = GetSeatid(buffer);

           _writer.Write(new ReadOnlySpan<byte>(System.Text.Encoding.ASCII.GetBytes(seatId.ToString())));
           _writer.Write(new ReadOnlySpan<byte>(System.Text.Encoding.ASCII.GetBytes("\n")));
    }
    }
}