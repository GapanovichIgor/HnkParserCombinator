namespace ParserCombinator

open System
open Utils

type Tape<'a>(initialBufferSize, getNext: unit -> 'a option) =
    let mutable buffer =
        Array.zeroCreate<'a> initialBufferSize

    let mutable windowStart = 0
    let mutable windowEnd = 0
    let mutable headPos = -1
    let mutable eos = false

    let allocate count =
        assert (count >= 0)

        let windowLength = windowEnd - windowStart

        if windowStart >= count then
            Array.Copy(buffer, windowStart, buffer, 0, windowLength)
            headPos <- headPos - windowStart
            windowEnd <- windowEnd - windowStart
            windowStart <- 0
        else
            let newLength =
                Math.Max(buffer.Length * 2, buffer.Length + count)

            let newBuffer =
                Array.zeroCreate<'a> newLength

            Array.Copy(buffer, windowStart, newBuffer, 0, windowLength)
            buffer <- newBuffer

    let tryRead count =
        assert (count >= 0)

        if eos then
            0
        else
            let bufferLengthIncrease =
                windowEnd + count - buffer.Length

            if bufferLengthIncrease > 0 then
                allocate bufferLengthIncrease

            let mutable i = 0

            while not eos && i < count do
                match getNext () with
                | Some item ->
                    buffer[windowEnd] <- item
                    windowEnd <- windowEnd + 1
                    i <- i + 1
                | None -> eos <- true

            i

    member _.Current: 'a option =

        let missingItemCount =
            headPos - windowEnd + 1

        if missingItemCount > 0
           && tryRead missingItemCount <> missingItemCount then
            None
        else
            assert (headPos >= windowStart)
            assert (headPos <= windowEnd)
            buffer[headPos] |> Some

    member _.MoveForward(count) : unit =
        assert (count >= 0)
        headPos <- headPos + count

    member this.MoveNext() : unit = this.MoveForward(1)

    member _.MoveBack(count) =
        assert (count >= 0)
        headPos <- headPos - count
        assert (headPos >= windowStart - 1)

    member _.Consume(count) : 'a array =
        assert (count >= 0)

        let result =
            buffer |> getSubArray (headPos + 1) count

        headPos <- headPos + count
        assert (headPos <= windowEnd)

        result

    member _.Commit(count) : unit =
        assert (count >= 0)
        windowStart <- windowStart + count
        assert (windowStart <= windowEnd)
