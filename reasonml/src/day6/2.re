type block = int;

type blocks = array(block);

type indexedBlock = (int, int);

type indexedBlocks = array(indexedBlock);

let rawInput = "2\t8\t8\t5\t4\t2\t3\t1\t5\t5\t1\t2\t15\t13\t5\t14";

let parsedInput =
  rawInput |> Js.String.split("\t") |> Array.map(int_of_string);

let addIndex = (blocks: blocks) : indexedBlocks =>
  blocks |> Array.mapi((index, block) => (index, block));

let getMaxBlock = (blocks: blocks) : indexedBlock =>
  Array.fold_left(
    ((maxIndex, maxValue), (nextIndex, nextValue)) =>
      if (maxValue >= nextValue) {
        (maxIndex, maxValue);
      } else {
        (nextIndex, nextValue);
      },
    addIndex(blocks)[0],
    addIndex(blocks)
  );

let incrementBlock = (index: int, blocks: blocks) : blocks => {
  blocks[index] = blocks[index] + 1;
  blocks;
};

let incrementBlock = (index: int, blocks: blocks) =>
  Array.copy(blocks) |> incrementBlock(index);

let getNextIteration = blocks : blocks => {
  let (maxIndex, maxValue) = getMaxBlock(blocks);
  let zeroedBlocks =
    blocks
    |> Array.copy
    |> (
      copy => {
        copy[maxIndex] = 0;
        copy;
      }
    );
  let numBlocks = Array.length(blocks);
  let rec distributeValues = (blocks, currentBlock, counter) =>
    if (counter == 0) {
      blocks;
    } else {
      distributeValues(
        incrementBlock(currentBlock, blocks),
        (currentBlock + 1) mod numBlocks,
        counter - 1
      );
    };
  distributeValues(zeroedBlocks, (maxIndex + 1) mod numBlocks, maxValue);
};

let rec findRepeatedBlock =
        (seen: list(blocks), currentIteration: blocks)
        : blocks =>
  if (List.mem(getNextIteration(currentIteration), seen)) {
    getNextIteration(currentIteration);
  } else {
    findRepeatedBlock(
      List.append(seen, [getNextIteration(currentIteration)]),
      getNextIteration(currentIteration)
    );
  };

let rec countLoopSize = (repeatStart: blocks) => {
  let rec iter = currentIteration =>
    if (currentIteration == repeatStart) {
      1;
    } else {
      1 + iter(getNextIteration(currentIteration));
    };
  iter(getNextIteration(repeatStart));
};

parsedInput |> findRepeatedBlock([parsedInput]) |> countLoopSize |> Js.log;