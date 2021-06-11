{-# LANGUAGE QuasiQuotes #-}
module Integrations.Rust.Benchmark where

import Ohua.Prelude  ( ($), Monad((>>=)), (=<<) )
import Integrations.Rust.Utils

spec :: Spec
spec =
    describe "Benchmark" $ do
        it "labyrinth" $
            -- the function below is closely resembling the naive sequential implementation: 
            -- https://github.com/Feliix42/ohua-rust-benchmarks/blob/master/labyrinth/src/bin/simple_sequential.rs#L122-L133
            -- upon writing the snippet below I realized that we might need to alter the code a bit for the "retry" semantics introduced with the batching transformation
            (showCode "Compiled: " =<< compileCode [sourceFile|
                use funs::*;

                fn route_paths(mut maze: Maze, to_map: Vec<(Point, Point)>) -> Maze {
                    for pair in to_map {
                        let mapped = find_path(pair, &maze.grid);
                        if mapped.is_some() {
                            let path = mapped.unwrap();
                            // this is the implementation in the original, but something like this is also possible I reckon:
                            // maze.update(path);
                            update_maze(&mut maze, path);
                        } else {
                            // Same here: The specification could most certainly altered to exclude the direct struct member access with something like:
                            // maze.add_unmappable(pair);
                            maze.unmappable_paths.push(pair);
                        }
                    }
                    
                    maze
                }
                |]) >>=
            (\compiled -> do
                expected <- showCode "Expected:"
                    [sourceFile|
                        use funs::*;

                        fn test(i: i32) -> i32 {
                            TODO
                        }
                    |]
                compiled `shouldBe` expected)
