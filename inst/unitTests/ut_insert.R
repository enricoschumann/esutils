checkEquals(insert(1:5, 99, 1), c(99, 1:5))
checkEquals(insert(1:5, 99, 6), c(1:5, 99))
checkEquals(insert(1:5, 99, 3), c(1:2, 99, 3:5))

checkEquals(insert(1:5, 98:99, c(1,5)), c(98, 1:3, 99, 4:5))
checkEquals(insert(1:5, 101:105, 1:5), c(101:105, 1:5))
