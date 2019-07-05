//
//  IllegalInstructionFour.swift
//  BugExpose
//
//  Created by masukomi on 1/24/16.
//

import Foundation

protocol IllegalInstructionFour {
    func isTrue() -> Bool;
}

extension IllegalInstructionFour {
    func getLowOrHighNum(numA: Int?, numB: Int?, lowOrHigh: String) -> Int? {
        let numArray =  [numA, numB].filter({ $0 != nil});
        return numArray.sort(
            lowOrHigh == "low" ? { $0! < $1} : {$0! > $1 }
            ).first!!;
    }
    

}