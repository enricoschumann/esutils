backup_dir <- file.path(tempdir(),
                        paste0("esutils_backups_", sample(1e9, 1L)))
dir.create(backup_dir)

d <- seq(Sys.Date()-366*10, Sys.Date(), by = "1 day")
for (i in seq_along(d)) {
    writeLines(as.character(i),
               file.path(
                   backup_dir,
                   format(d[i], "%Y%m%d_120000__es-ch____X.txt")))
}

n <- length(list.files(backup_dir))

removed <- file.remove(old_files(path = backup_dir,
                                 min.age = 60,
                                 min.age.monthend = 31*12,
                                 min.age.yearend  = 366*5,
                                 full.names = TRUE))

pruned <- length(list.files(backup_dir))

expect_true(pruned < n)

removed <- file.remove(old_files(path = backup_dir,
                                 min.age = 60,
                                 min.age.monthend = 31*12,
                                 min.age.yearend  = 366*5,
                                 full.names = TRUE))

pruned2 <- length(list.files(backup_dir))

expect_true(pruned == pruned2)
expect_true(pruned2 <= 60 + 12 + 5)  ## 60 days + 12 months + 5 years
expect_true(pruned2 >= 60 +  9 + 4)  ## a lower bound: 3 months, 1 yearend in last 60 days

unlink(backup_dir, recursive = TRUE)
