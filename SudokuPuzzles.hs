module SudokuPuzzles where

import Data.Matrix as M

easySudoku :: Matrix [Int]
easySudoku = fromList 9 9 [[],[],[3],[],[],[],[],[],[7],
                           [4],[2],[],[],[9],[1],[],[],[],
                           [],[1],[5],[],[6],[8],[3],[],[],
                           [8],[7],[],[],[3],[4],[9],[],[6],
                           [3],[],[],[],[],[],[],[5],[1],
                           [2],[],[6],[],[8],[9],[4],[],[3],
                           [5],[9],[],[2],[1],[6],[],[],[],
                           [1],[],[2],[9],[],[7],[],[8],[],
                           [7],[],[],[],[5],[],[],[],[]]


hardSudoku :: Matrix [Int]
hardSudoku = fromLists [[[],[2],[],[],[5],[],[8],[],[]],
                        [[4],[],[],[],[1],[],[2],[],[]],
                        [[],[],[],[],[],[],[],[4],[]],
                        [[6],[],[5],[],[],[7],[1],[],[]],
                        [[],[3],[4],[],[2],[6],[],[],[]],
                        [[],[],[],[],[],[],[7],[],[]],
                        [[3],[],[],[],[7],[],[],[1],[5]],
                        [[1],[],[],[],[4],[],[],[],[3]],
                        [[],[6],[],[],[],[],[],[2],[]]]

hardSudoku2 :: Matrix [Int]
hardSudoku2 = fromLists [[[1],[],[],[6],[],[],[],[8],[]],
                        [[],[4],[],[],[],[],[],[2],[]],
                        [[5],[],[],[],[9],[],[4],[],[6]],
                        [[],[5],[],[2],[],[],[7],[],[1]],
                        [[9],[],[],[],[],[3],[],[],[]],
                        [[],[],[],[],[],[],[],[4],[]],
                        [[],[],[1],[],[2],[],[],[],[]],
                        [[],[],[],[],[],[],[],[],[8]],
                        [[],[7],[],[1],[],[],[6],[],[5]]]

hardSudoku3 :: Matrix [Int]
hardSudoku3 = fromLists [[[],[6],[],[1],[],[],[],[8],[]],
                [[5],[],[],[],[],[],[],[2],[]],
                [[],[],[7],[],[4],[],[1],[],[5]],
                [[],[],[],[],[],[],[],[1],[]],
                [[8],[],[],[4],[],[],[6],[],[9]],
                [[],[3],[],[],[7],[],[],[],[]],
                [[9],[],[],[6],[],[],[5],[],[4]],
                [[],[],[8],[],[],[9],[],[],[]],
                [[],[],[],[],[],[],[],[],[2]]]

hardSudoku4 :: Matrix [Int]
hardSudoku4 = fromLists [[[],[],[3],[5],[],[],[],[],[9]],
                [[],[8],[],[2],[],[],[6],[3],[]],
                [[],[],[],[],[],[6],[],[],[4]],
                [[],[4],[],[],[5],[],[],[],[]],
                [[],[],[],[],[],[],[],[9],[]],
                [[],[],[5],[],[],[7],[3],[2],[]],
                [[1],[],[],[8],[],[],[],[],[]],
                [[],[],[],[],[],[],[],[],[6]],
                [[],[],[4],[],[],[2],[7],[5],[]]]

hardestSudoku :: Matrix [Int]
hardestSudoku = fromLists [[[8],[],[],[],[],[],[],[],[]],
                                    [[],[],[3],[6],[],[],[],[],[]],
                                    [[],[7],[],[],[9],[],[2],[],[]],
                                    [[],[5],[],[],[],[7],[],[],[]],
                                    [[],[],[],[],[4],[5],[7],[],[]],
                                    [[],[],[],[1],[],[],[],[3],[]],
                                    [[],[],[1],[],[],[],[],[6],[8]],
                                    [[],[],[8],[5],[],[],[],[1],[]],
                                    [[],[9],[],[],[],[],[4],[],[]]]                

sixteenSud :: Matrix [Int]
sixteenSud = fromLists [[[],[10],[13],[],[8],[16],[3],[7],[],[14],[],[4],[5],[],[12],[]],
                    [[7],[15],[5],[],[14],[2],[],[],[12],[],[10],[6],[],[11],[],[9]],
                    [[11],[6],[1],[4],[10],[],[],[15],[],[16],[9],[],[2],[],[],[14]],
                    [[],[],[12],[14],[],[9],[],[],[],[1],[5],[],[10],[7],[4],[15]],
                    [[8],[2],[7],[],[4],[],[9],[10],[11],[13],[3],[],[],[1],[],[12]],
                    [[12],[1],[],[9],[],[3],[],[6],[],[4],[],[14],[],[16],[5],[8]],
                    [[4],[],[],[],[1],[],[],[5],[],[9],[],[],[],[2],[3],[10]],
                    [[10],[],[3],[],[12],[7],[16],[13],[],[2],[8],[1],[14],[4],[],[]],
                    [[],[8],[],[],[11],[],[],[],[],[],[6],[9],[12],[10],[],[]],
                    [[13],[],[4],[6],[16],[15],[8],[14],[],[10],[12],[11],[],[5],[2],[7]],
                    [[],[7],[10],[11],[6],[],[],[12],[2],[5],[13],[],[],[8],[],[3]],
                    [[3],[12],[],[],[],[5],[],[2],[14],[8],[],[7],[15],[],[],[6]],
                    [[5],[],[9],[12],[],[],[],[16],[13],[],[],[8],[11],[14],[6],[]],
                    [[],[16],[],[7],[5],[4],[14],[1],[9],[11],[2],[],[8],[12],[10],[]],
                    [[2],[],[],[3],[],[6],[13],[],[],[12],[],[],[4],[15],[],[16]],
                    [[],[13],[14],[10],[2],[11],[12],[],[],[6],[16],[15],[],[],[7],[]]]



fourSudoku :: Matrix [Int]
fourSudoku = fromLists [[[1],[],[],[]],
                [[3],[],[],[]],
                [[],[],[],[4]],
                [[],[],[],[3]]]

fourSudoku2 :: Matrix [Int]
fourSudoku2 = fromLists [[[],[],[1],[]],
                [[4],[],[],[]],
                [[],[],[],[2]],
                [[],[3],[],[]]]


