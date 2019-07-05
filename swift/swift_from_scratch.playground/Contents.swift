//: Playground - noun: a place where people can play

import Cocoa;
import Foundation;


import CoreFoundation

let char1: Character = "A"
let char2: Character = "B"

//print("Value of char1 \(char1)")
let splitable_things = " \\s+,;/\\:._{}\\[\\]()\'\"\\|\\-=\\~>`<\\\\"
let charSet = Set([Character](" \\s+,;/\\:._{}\\[\\]()\'\"\\|\\-=\\~>`<\\\\".characters))
let idx = charSet.index(of: Character("{"))


//let x = " \\s+,;/\\:._{}\\[\\]()\'\"\\|\\-=\\~>`<\\\\".characters.split(Character);

var strings = Set<String>();


strings.insert("a")
strings.insert("b");

strings.contains("c");
strings.contains("a");
strings.joined(separator: "")


var stringsA = Array<Character>();
stringsA.append("x");
stringsA.append("y");
var z = stringsA.map({String($0)}).joined(separator: "");
z

let underFive = 0.0..<5.0
underFive.
