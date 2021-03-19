expect_equal(insert(1:5, 99, 1), c(99, 1:5))
expect_equal(insert(1:5, 99, 6), c(1:5, 99))
expect_equal(insert(1:5, 99, 3), c(1:2, 99, 3:5))

expect_equal(insert(1:5, 98:99, c(1,5)), c(98, 1:4, 99, 5))
expect_equal(insert(1:3, 101:103, 1:3), c(101,1,102,2,103,3))
