
R version 3.4.1 (2017-06-30) -- "Single Candle"
Copyright (C) 2017 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

> logitMod <- glm(Y ~ X1 + X2, family="binomial", data = training
+ -
+ l
+ ;
Error: unexpected ';' in:
"l
;"
> logitMod <- glm(Y ~ X1 + X2, family="binomial", data = trainingData)
Error in is.data.frame(data) : object 'trainingData' not found
> predictedY <- predict(logitMod, testData, type="response") 
Error in predict(logitMod, testData, type = "response") : 
  object 'logitMod' not found
> utils:::menuInstallPkgs()
--- Please select a CRAN mirror for use in this session ---
trying URL 'https://cloud.r-project.org/bin/windows/contrib/3.4/mlbench_2.1-1.zip'
Content type 'application/zip' length 1033792 bytes (1009 KB)
downloaded 1009 KB

package ‘mlbench’ successfully unpacked and MD5 sums checked

The downloaded binary packages are in
        C:\Users\xxxx\AppData\Local\Temp\RtmpMXDUgl\downloaded_packages
> data(BreastCancer, package="mlbench")
> bc <- BreastCancer[complete.cases(BreastCancer), ]
> data
function (..., list = character(), package = NULL, lib.loc = NULL, 
    verbose = getOption("verbose"), envir = .GlobalEnv) 
{
    fileExt <- function(x) {
        db <- grepl("\\\\.[^.]+\\\\.(gz|bz2|xz)$", x)
        ans <- sub(".*\\\\.", "", x)
        ans[db] <- sub(".*\\\\.([^.]+\\\\.)(gz|bz2|xz)$", "\\\\1\\\\2", 
            x[db])
        ans
    }
    names <- c(as.character(substitute(list(...))[-1L]), list)
    if (!is.null(package)) {
        if (!is.character(package)) 
            stop("'package' must be a character string or NULL")
        if (any(package %in% "base")) 
            warning("datasets have been moved from package 'base' to package 'datasets'")
        if (any(package %in% "stats")) 
            warning("datasets have been moved from package 'stats' to package 'datasets'")
        package[package %in% c("base", "stats")] <- "datasets"
    }
    paths <- find.package(package, lib.loc, verbose = verbose)
    if (is.null(lib.loc)) 
        paths <- c(path.package(package, TRUE), if (!length(package)) getwd(), 
            paths)
    paths <- unique(normalizePath(paths[file.exists(paths)]))
    paths <- paths[dir.exists(file.path(paths, "data"))]
    dataExts <- tools:::.make_file_exts("data")
    if (length(names) == 0L) {
        db <- matrix(character(), nrow = 0L, ncol = 4L)
        for (path in paths) {
            entries <- NULL
            packageName <- if (file_test("-f", file.path(path, 
                "DESCRIPTION"))) 
                basename(path)
            else "."
            if (file_test("-f", INDEX <- file.path(path, "Meta", 
                "data.rds"))) {
                entries <- readRDS(INDEX)
            }
            else {
                dataDir <- file.path(path, "data")
                entries <- tools::list_files_with_type(dataDir, 
                  "data")
                if (length(entries)) {
                  entries <- unique(tools::file_path_sans_ext(basename(entries)))
                  entries <- cbind(entries, "")
                }
            }
            if (NROW(entries)) {
                if (is.matrix(entries) && ncol(entries) == 2L) 
                  db <- rbind(db, cbind(packageName, dirname(path), 
                    entries))
                else warning(gettextf("data index for package %s is invalid and will be ignored", 
                  sQuote(packageName)), domain = NA, call. = FALSE)
            }
        }
        colnames(db) <- c("Package", "LibPath", "Item", "Title")
        footer <- if (missing(package)) 
            paste0("Use ", sQuote(paste("data(package =", ".packages(all.available = TRUE))")), 
                "\\n", "to list the data sets in all *available* packages.")
        else NULL
        y <- list(title = "Data sets", header = NULL, results = db, 
            footer = footer)
        class(y) <- "packageIQR"
        return(y)
    }
    paths <- file.path(paths, "data")
    for (name in names) {
        found <- FALSE
        for (p in paths) {
            if (file_test("-f", file.path(p, "Rdata.rds"))) {
                rds <- readRDS(file.path(p, "Rdata.rds"))
                if (name %in% names(rds)) {
                  found <- TRUE
                  if (verbose) 
                    message(sprintf("name=%s:\\t found in Rdata.rds", 
                      name), domain = NA)
                  thispkg <- sub(".*/([^/]*)/data$", "\\\\1", p)
                  thispkg <- sub("_.*$", "", thispkg)
                  thispkg <- paste0("package:", thispkg)
                  objs <- rds[[name]]
                  lazyLoad(file.path(p, "Rdata"), envir = envir, 
                    filter = function(x) x %in% objs)
                  break
                }
                else if (verbose) 
                  message(sprintf("name=%s:\\t NOT found in names() of Rdata.rds, i.e.,\\n\\t%s\\n", 
                    name, paste(names(rds), collapse = ",")), 
                    domain = NA)
            }
            if (file_test("-f", file.path(p, "Rdata.zip"))) {
                warning("zipped data found for package ", sQuote(basename(dirname(p))), 
                  ".\\nThat is defunct, so please re-install the package.", 
                  domain = NA)
                if (file_test("-f", fp <- file.path(p, "filelist"))) 
                  files <- file.path(p, scan(fp, what = "", quiet = TRUE))
                else {
                  warning(gettextf("file 'filelist' is missing for directory %s", 
                    sQuote(p)), domain = NA)
                  next
                }
            }
            else {
                files <- list.files(p, full.names = TRUE)
            }
            files <- files[grep(name, files, fixed = TRUE)]
            if (length(files) > 1L) {
                o <- match(fileExt(files), dataExts, nomatch = 100L)
                paths0 <- dirname(files)
                paths0 <- factor(paths0, levels = unique(paths0))
                files <- files[order(paths0, o)]
            }
            if (length(files)) {
                for (file in files) {
                  if (verbose) 
                    message("name=", name, ":\\t file= ...", .Platform$file.sep, 
                      basename(file), "::\\t", appendLF = FALSE, 
                      domain = NA)
                  ext <- fileExt(file)
                  if (basename(file) != paste0(name, ".", ext)) 
                    found <- FALSE
                  else {
                    found <- TRUE
                    zfile <- file
                    zipname <- file.path(dirname(file), "Rdata.zip")
                    if (file.exists(zipname)) {
                      Rdatadir <- tempfile("Rdata")
                      dir.create(Rdatadir, showWarnings = FALSE)
                      topic <- basename(file)
                      rc <- .External(C_unzip, zipname, topic, 
                        Rdatadir, FALSE, TRUE, FALSE, FALSE)
                      if (rc == 0L) 
                        zfile <- file.path(Rdatadir, topic)
                    }
                    if (zfile != file) 
                      on.exit(unlink(zfile))
                    switch(ext, R = , r = {
                      library("utils")
                      sys.source(zfile, chdir = TRUE, envir = envir)
                    }, RData = , rdata = , rda = load(zfile, 
                      envir = envir), TXT = , txt = , tab = , 
                      tab.gz = , tab.bz2 = , tab.xz = , txt.gz = , 
                      txt.bz2 = , txt.xz = assign(name, read.table(zfile, 
                        header = TRUE, as.is = FALSE), envir = envir), 
                      CSV = , csv = , csv.gz = , csv.bz2 = , 
                      csv.xz = assign(name, read.table(zfile, 
                        header = TRUE, sep = ";", as.is = FALSE), 
                        envir = envir), found <- FALSE)
                  }
                  if (found) 
                    break
                }
                if (verbose) 
                  message(if (!found) 
                    "*NOT* ", "found", domain = NA)
            }
            if (found) 
                break
        }
        if (!found) 
            warning(gettextf("data set %s not found", sQuote(name)), 
                domain = NA)
    }
    invisible(names)
}
<bytecode: 0x0000000006d82560>
<environment: namespace:utils>
> bc
          Id Cl.thickness Cell.size Cell.shape Marg.adhesion Epith.c.size
1    1000025            5         1          1             1            2
2    1002945            5         4          4             5            7
3    1015425            3         1          1             1            2
4    1016277            6         8          8             1            3
5    1017023            4         1          1             3            2
6    1017122            8        10         10             8            7
7    1018099            1         1          1             1            2
8    1018561            2         1          2             1            2
9    1033078            2         1          1             1            2
10   1033078            4         2          1             1            2
11   1035283            1         1          1             1            1
12   1036172            2         1          1             1            2
13   1041801            5         3          3             3            2
14   1043999            1         1          1             1            2
15   1044572            8         7          5            10            7
16   1047630            7         4          6             4            6
17   1048672            4         1          1             1            2
18   1049815            4         1          1             1            2
19   1050670           10         7          7             6            4
20   1050718            6         1          1             1            2
21   1054590            7         3          2            10            5
22   1054593           10         5          5             3            6
23   1056784            3         1          1             1            2
25   1059552            1         1          1             1            2
26   1065726            5         2          3             4            2
27   1066373            3         2          1             1            1
28   1066979            5         1          1             1            2
29   1067444            2         1          1             1            2
30   1070935            1         1          3             1            2
31   1070935            3         1          1             1            1
32   1071760            2         1          1             1            2
33   1072179           10         7          7             3            8
34   1074610            2         1          1             2            2
35   1075123            3         1          2             1            2
36   1079304            2         1          1             1            2
37   1080185           10        10         10             8            6
38   1081791            6         2          1             1            1
39   1084584            5         4          4             9            2
40   1091262            2         5          3             3            6
42   1099510           10         4          3             1            3
43   1100524            6        10         10             2            8
44   1102573            5         6          5             6           10
45   1103608           10        10         10             4            8
46   1103722            1         1          1             1            2
47   1105257            3         7          7             4            4
48   1105524            1         1          1             1            2
49   1106095            4         1          1             3            2
50   1106829            7         8          7             2            4
51   1108370            9         5          8             1            2
52   1108449            5         3          3             4            2
53   1110102           10         3          6             2            3
54   1110503            5         5          5             8           10
55   1110524           10         5          5             6            8
56   1111249           10         6          6             3            4
57   1112209            8        10         10             1            3
58   1113038            8         2          4             1            5
59   1113483            5         2          3             1            6
60   1113906            9         5          5             2            2
61   1115282            5         3          5             5            3
62   1115293            1         1          1             1            2
63   1116116            9        10         10             1           10
64   1116132            6         3          4             1            5
65   1116192            1         1          1             1            2
66   1116998           10         4          2             1            3
67   1117152            4         1          1             1            2
68   1118039            5         3          4             1            8
69   1120559            8         3          8             3            4
70   1121732            1         1          1             1            2
71   1121919            5         1          3             1            2
72   1123061            6        10          2             8           10
73   1124651            1         3          3             2            2
74   1125035            9         4          5            10            6
75   1126417           10         6          4             1            3
76   1131294            1         1          2             1            2
77   1132347            1         1          4             1            2
78   1133041            5         3          1             2            2
79   1133136            3         1          1             1            2
80   1136142            2         1          1             1            3
81   1137156            2         2          2             1            1
82   1143978            4         1          1             2            2
83   1143978            5         2          1             1            2
84   1147044            3         1          1             1            2
85   1147699            3         5          7             8            8
86   1147748            5        10          6             1           10
87   1148278            3         3          6             4            5
88   1148873            3         6          6             6            5
89   1152331            4         1          1             1            2
90   1155546            2         1          1             2            3
91   1156272            1         1          1             1            2
92   1156948            3         1          1             2            2
93   1157734            4         1          1             1            2
94   1158247            1         1          1             1            2
95   1160476            2         1          1             1            2
96   1164066            1         1          1             1            2
97   1165297            2         1          1             2            2
98   1165790            5         1          1             1            2
99   1165926            9         6          9             2           10
100  1166630            7         5          6            10            5
101  1166654           10         3          5             1           10
102  1167439            2         3          4             4            2
103  1167471            4         1          2             1            2
104  1168359            8         2          3             1            6
105  1168736           10        10         10            10           10
106  1169049            7         3          4             4            3
107  1170419           10        10         10             8            2
108  1170420            1         6          8            10            8
109  1171710            1         1          1             1            2
110  1171710            6         5          4             4            3
111  1171795            1         3          1             2            2
112  1171845            8         6          4             3            5
113  1172152           10         3          3            10            2
114  1173216           10        10         10             3           10
115  1173235            3         3          2             1            2
116  1173347            1         1          1             1            2
117  1173347            8         3          3             1            2
118  1173509            4         5          5            10            4
119  1173514            1         1          1             1            4
120  1173681            3         2          1             1            2
121  1174057            1         1          2             2            2
122  1174057            4         2          1             1            2
123  1174131           10        10         10             2           10
124  1174428            5         3          5             1            8
125  1175937            5         4          6             7            9
126  1176406            1         1          1             1            2
127  1176881            7         5          3             7            4
128  1177027            3         1          1             1            2
129  1177399            8         3          5             4            5
130  1177512            1         1          1             1           10
131  1178580            5         1          3             1            2
132  1179818            2         1          1             1            2
133  1180194            5        10          8            10            8
134  1180523            3         1          1             1            2
135  1180831            3         1          1             1            3
136  1181356            5         1          1             1            2
137  1182404            4         1          1             1            2
138  1182410            3         1          1             1            2
139  1183240            4         1          2             1            2
141  1183516            3         1          1             1            2
142  1183911            2         1          1             1            2
143  1183983            9         5          5             4            4
144  1184184            1         1          1             1            2
145  1184241            2         1          1             1            2
147  1185609            3         4          5             2            6
148  1185610            1         1          1             1            3
149  1187457            3         1          1             3            8
150  1187805            8         8          7             4           10
151  1188472            1         1          1             1            1
152  1189266            7         2          4             1            6
153  1189286           10        10          8             6            4
154  1190394            4         1          1             1            2
155  1190485            1         1          1             1            2
156  1192325            5         5          5             6            3
157  1193091            1         2          2             1            2
158  1193210            2         1          1             1            2
160  1196295            9         9         10             3            6
161  1196915           10         7          7             4            5
162  1197080            4         1          1             1            2
163  1197270            3         1          1             1            2
164  1197440            1         1          1             2            1
166  1197979            4         1          1             1            2
167  1197993            5         6          7             8            8
168  1198128           10         8         10            10            6
169  1198641            3         1          1             1            2
170  1199219            1         1          1             2            1
171  1199731            3         1          1             1            2
172  1199983            1         1          1             1            2
173  1200772            1         1          1             1            2
174  1200847            6        10         10            10            8
175  1200892            8         6          5             4            3
176  1200952            5         8          7             7           10
177  1201834            2         1          1             1            2
178  1201936            5        10         10             3            8
179  1202125            4         1          1             1            2
180  1202812            5         3          3             3            6
181  1203096            1         1          1             1            1
182  1204242            1         1          1             1            2
183  1204898            6         1          1             1            2
184  1205138            5         8          8             8            5
185  1205579            8         7          6             4            4
186  1206089            2         1          1             1            1
187  1206695            1         5          8             6            5
188  1206841           10         5          6            10            6
189  1207986            5         8          4            10            5
190  1208301            1         2          3             1            2
191  1210963           10        10         10             8            6
192  1211202            7         5         10            10           10
193  1212232            5         1          1             1            2
194  1212251            1         1          1             1            2
195  1212422            3         1          1             1            2
196  1212422            4         1          1             1            2
197  1213375            8         4          4             5            4
198  1213383            5         1          1             4            2
199  1214092            1         1          1             1            2
200  1214556            3         1          1             1            2
201  1214966            9         7          7             5            5
202  1216694           10         8          8             4           10
203  1216947            1         1          1             1            2
204  1217051            5         1          1             1            2
205  1217264            1         1          1             1            2
206  1218105            5        10         10             9            6
207  1218741           10        10          9             3            7
208  1218860            1         1          1             1            1
209  1218860            1         1          1             1            1
210  1219406            5         1          1             1            1
211  1219525            8        10         10            10            5
212  1219859            8        10          8             8            4
213  1220330            1         1          1             1            2
214  1221863           10        10         10            10            7
215  1222047           10        10         10            10            3
216  1222936            8         7          8             7            5
217  1223282            1         1          1             1            2
218  1223426            1         1          1             1            2
219  1223793            6        10          7             7            6
220  1223967            6         1          3             1            2
221  1224329            1         1          1             2            2
222  1225799           10         6          4             3           10
223  1226012            4         1          1             3            1
224  1226612            7         5          6             3            3
225  1227210           10         5          5             6            3
226  1227244            1         1          1             1            2
227  1227481           10         5          7             4            4
228  1228152            8         9          9             5            3
229  1228311            1         1          1             1            1
230  1230175           10        10         10             3           10
231  1230688            7         4          7             4            3
232  1231387            6         8          7             5            6
233  1231706            8         4          6             3            3
234  1232225           10         4          5             5            5
235  1236043            3         3          2             1            3
237  1241559           10         8          8             2            8
238  1241679            9         8          8             5            6
239  1242364            8        10         10             8            6
240  1243256           10         4          3             2            3
241  1270479            5         1          3             3            2
242  1276091            3         1          1             3            1
243  1277018            2         1          1             1            2
244   128059            1         1          1             1            2
245  1285531            1         1          1             1            2
246  1287775            5         1          1             2            2
247   144888            8        10         10             8            5
248   145447            8         4          4             1            2
249   167528            4         1          1             1            2
251   183913            1         2          2             1            2
252   191250           10         4          4            10            2
253  1017023            6         3          3             5            3
254  1100524            6        10         10             2            8
255  1116116            9        10         10             1           10
256  1168736            5         6          6             2            4
257  1182404            3         1          1             1            2
258  1182404            3         1          1             1            2
259  1198641            3         1          1             1            2
260   242970            5         7          7             1            5
261   255644           10         5          8            10            3
262   263538            5        10         10             6           10
263   274137            8         8          9             4            5
264   303213           10         4          4            10            6
265   314428            7         9          4            10           10
266  1182404            5         1          4             1            2
267  1198641           10        10          6             3            3
268   320675            3         3          5             2            3
269   324427           10         8          8             2            3
270   385103            1         1          1             1            2
271   390840            8         4          7             1            3
272   411453            5         1          1             1            2
273   320675            3         3          5             2            3
274   428903            7         2          4             1            3
275   431495            3         1          1             1            2
277   434518            3         1          1             1            2
278   452264            1         1          1             1            2
279   456282            1         1          1             1            2
280   476903           10         5          7             3            3
281   486283            3         1          1             1            2
282   486662            2         1          1             2            2
283   488173            1         4          3            10            4
284   492268           10         4          6             1            2
285   508234            7         4          5            10            2
286   527363            8        10         10            10            8
287   529329           10        10         10            10           10
288   535331            3         1          1             1            3
289   543558            6         1          3             1            4
290   555977            5         6          6             8            6
291   560680            1         1          1             1            2
292   561477            1         1          1             1            2
294   601265           10         4          4             6            2
296   606722            5         5          7             8            6
297   616240            5         3          4             3            4
299   625201            8         2          1             1            5
300    63375            9         1          2             6            4
301   635844            8         4         10             5            4
302   636130            1         1          1             1            2
303   640744           10        10         10             7            9
304   646904            1         1          1             1            2
305   653777            8         3          4             9            3
306   659642           10         8          4             4            4
307   666090            1         1          1             1            2
308   666942            1         1          1             1            2
309   667204            7         8          7             6            4
310   673637            3         1          1             1            2
311   684955            2         1          1             1            3
312   688033            1         1          1             1            2
313   691628            8         6          4            10           10
314   693702            1         1          1             1            2
315   704097            1         1          1             1            1
317   706426            5         5          5             2            5
318   709287            6         8          7             8            6
319   718641            1         1          1             1            5
320   721482            4         4          4             4            6
321   730881            7         6          3             2            5
323   733639            3         1          1             1            2
324   733823            5         4          6            10            2
325   740492            1         1          1             1            2
326   743348            3         2          2             1            2
327   752904           10         1          1             1            2
328   756136            1         1          1             1            2
329   760001            8        10          3             2            6
330   760239           10         4          6             4            5
331    76389           10         4          7             2            2
332   764974            5         1          1             1            2
333   770066            5         2          2             2            2
334   785208            5         4          6             6            4
335   785615            8         6          7             3            3
336   792744            1         1          1             1            2
337   797327            6         5          5             8            4
338   798429            1         1          1             1            2
339   704097            1         1          1             1            1
340   806423            8         5          5             5            2
341   809912           10         3          3             1            2
342   810104            1         1          1             1            2
343   814265            2         1          1             1            2
344   814911            1         1          1             1            2
345   822829            7         6          4             8           10
346   826923            1         1          1             1            2
347   830690            5         2          2             2            3
348   831268            1         1          1             1            1
349   832226            3         4          4            10            5
350   832567            4         2          3             5            3
351   836433            5         1          1             3            2
352   837082            2         1          1             1            2
353   846832            3         4          5             3            7
354   850831            2         7         10            10            7
355   855524            1         1          1             1            2
356   857774            4         1          1             1            3
357   859164            5         3          3             1            3
358   859350            8        10         10             7           10
359   866325            8        10          5             3            8
360   873549           10         3          5             4            3
361   877291            6        10         10            10           10
362   877943            3        10          3            10            6
363   888169            3         2          2             1            4
364   888523            4         4          4             2            2
365   896404            2         1          1             1            2
366   897172            2         1          1             1            2
367    95719            6        10         10            10            8
368   160296            5         8          8            10            5
369   342245            1         1          3             1            2
370   428598            1         1          3             1            1
371   492561            4         3          2             1            3
372   493452            1         1          3             1            2
373   493452            4         1          2             1            2
374   521441            5         1          1             2            2
375   560680            3         1          2             1            2
376   636437            1         1          1             1            2
377   640712            1         1          1             1            2
378   654244            1         1          1             1            1
379   657753            3         1          1             4            3
380   685977            5         3          4             1            4
381   805448            1         1          1             1            2
382   846423           10         6          3             6            4
383  1002504            3         2          2             2            2
384  1022257            2         1          1             1            2
385  1026122            2         1          1             1            2
386  1071084            3         3          2             2            3
387  1080233            7         6          6             3            2
388  1114570            5         3          3             2            3
389  1114570            2         1          1             1            2
390  1116715            5         1          1             1            3
391  1131411            1         1          1             2            2
392  1151734           10         8          7             4            3
393  1156017            3         1          1             1            2
394  1158247            1         1          1             1            1
395  1158405            1         2          3             1            2
396  1168278            3         1          1             1            2
397  1176187            3         1          1             1            2
398  1196263            4         1          1             1            2
399  1196475            3         2          1             1            2
400  1206314            1         2          3             1            2
401  1211265            3        10          8             7            6
402  1213784            3         1          1             1            2
403  1223003            5         3          3             1            2
404  1223306            3         1          1             1            2
405  1223543            1         2          1             3            2
406  1229929            1         1          1             1            2
407  1231853            4         2          2             1            2
408  1234554            1         1          1             1            2
409  1236837            2         3          2             2            2
410  1237674            3         1          2             1            2
411  1238021            1         1          1             1            2
413  1238633           10        10         10             6            8
414  1238915            5         1          2             1            2
415  1238948            8         5          6             2            3
416  1239232            3         3          2             6            3
417  1239347            8         7          8             5           10
418  1239967            1         1          1             1            2
419  1240337            5         2          2             2            2
420  1253505            2         3          1             1            5
421  1255384            3         2          2             3            2
422  1257200           10        10         10             7           10
423  1257648            4         3          3             1            2
424  1257815            5         1          3             1            2
425  1257938            3         1          1             1            2
426  1258549            9        10         10            10           10
427  1258556            5         3          6             1            2
428  1266154            8         7          8             2            4
429  1272039            1         1          1             1            2
430  1276091            2         1          1             1            2
431  1276091            1         3          1             1            2
432  1276091            5         1          1             3            4
433  1277629            5         1          1             1            2
434  1293439            3         2          2             3            2
435  1293439            6         9          7             5            5
436  1294562           10         8         10             1            3
437  1295186           10        10         10             1            6
438   527337            4         1          1             1            2
439   558538            4         1          3             3            2
440   566509            5         1          1             1            2
441   608157           10         4          3            10            4
442   677910            5         2          2             4            2
443   734111            1         1          1             3            2
444   734111            1         1          1             1            2
445   780555            5         1          1             6            3
446   827627            2         1          1             1            2
447  1049837            1         1          1             1            2
448  1058849            5         1          1             1            2
449  1182404            1         1          1             1            1
450  1193544            5         7          9             8            6
451  1201870            4         1          1             3            1
452  1202253            5         1          1             1            2
453  1227081            3         1          1             3            2
454  1230994            4         5          5             8            6
455  1238410            2         3          1             1            3
456  1246562           10         2          2             1            2
457  1257470           10         6          5             8            5
458  1259008            8         8          9             6            6
459  1266124            5         1          2             1            2
460  1267898            5         1          3             1            2
461  1268313            5         1          1             3            2
462  1268804            3         1          1             1            2
463  1276091            6         1          1             3            2
464  1280258            4         1          1             1            2
465  1293966            4         1          1             1            2
466  1296572           10         9          8             7            6
467  1298416           10         6          6             2            4
468  1299596            6         6          6             5            4
469  1105524            4         1          1             1            2
470  1181685            1         1          2             1            2
471  1211594            3         1          1             1            1
472  1238777            6         1          1             3            2
473  1257608            6         1          1             1            1
474  1269574            4         1          1             1            2
475  1277145            5         1          1             1            2
476  1287282            3         1          1             1            2
477  1296025            4         1          2             1            2
478  1296263            4         1          1             1            2
479  1296593            5         2          1             1            2
480  1299161            4         8          7            10            4
481  1301945            5         1          1             1            1
482  1302428            5         3          2             4            2
483  1318169            9        10         10            10           10
484   474162            8         7          8             5            5
485   787451            5         1          2             1            2
486  1002025            1         1          1             3            1
487  1070522            3         1          1             1            1
488  1073960           10        10         10            10            6
489  1076352            3         6          4            10            3
490  1084139            6         3          2             1            3
491  1115293            1         1          1             1            2
492  1119189            5         8          9             4            3
493  1133991            4         1          1             1            1
494  1142706            5        10         10            10            6
495  1155967            5         1          2            10            4
496  1170945            3         1          1             1            1
497  1181567            1         1          1             1            1
498  1182404            4         2          1             1            2
499  1204558            4         1          1             1            2
500  1217952            4         1          1             1            2
501  1224565            6         1          1             1            2
502  1238186            4         1          1             1            2
503  1253917            4         1          1             2            2
504  1265899            4         1          1             1            2
505  1268766            1         1          1             1            2
506  1277268            3         3          1             1            2
507  1286943            8        10         10            10            7
508  1295508            1         1          1             1            2
509  1297327            5         1          1             1            2
510  1297522            2         1          1             1            2
511  1298360            1         1          1             1            2
512  1299924            5         1          1             1            2
513  1299994            5         1          1             1            2
514  1304595            3         1          1             1            1
515  1306282            6         6          7            10            3
516  1313325            4        10          4             7            3
517  1320077            1         1          1             1            1
518  1320077            1         1          1             1            1
519  1320304            3         1          2             2            2
520  1330439            4         7          8             3            4
521   333093            1         1          1             1            3
522   369565            4         1          1             1            3
523   412300           10         4          5             4            3
524   672113            7         5          6            10            4
525   749653            3         1          1             1            2
526   769612            3         1          1             2            2
527   769612            4         1          1             1            2
528   798429            4         1          1             1            2
529   807657            6         1          3             2            2
530  8233704            4         1          1             1            1
531   837480            7         4          4             3            4
532   867392            4         2          2             1            2
533   869828            1         1          1             1            1
534  1043068            3         1          1             1            2
535  1056171            2         1          1             1            2
536  1061990            1         1          3             2            2
537  1113061            5         1          1             1            2
538  1116192            5         1          2             1            2
539  1135090            4         1          1             1            2
540  1145420            6         1          1             1            2
541  1158157            5         1          1             1            2
542  1171578            3         1          1             1            2
543  1174841            5         3          1             1            2
544  1184586            4         1          1             1            2
545  1186936            2         1          3             2            2
546  1197527            5         1          1             1            2
547  1222464            6        10         10            10            4
548  1240603            2         1          1             1            1
549  1240603            3         1          1             1            1
550  1241035            7         8          3             7            4
551  1287971            3         1          1             1            2
552  1289391            1         1          1             1            2
553  1299924            3         2          2             2            2
554  1306339            4         4          2             1            2
555  1313658            3         1          1             1            2
556  1313982            4         3          1             1            2
557  1321264            5         2          2             2            1
558  1321321            5         1          1             3            2
559  1321348            2         1          1             1            2
560  1321931            5         1          1             1            2
561  1321942            5         1          1             1            2
562  1321942            5         1          1             1            2
563  1328331            1         1          1             1            2
564  1328755            3         1          1             1            2
565  1331405            4         1          1             1            2
566  1331412            5         7         10            10            5
567  1333104            3         1          2             1            2
568  1334071            4         1          1             1            2
569  1343068            8         4          4             1            6
570  1343374           10        10          8            10            6
571  1344121            8        10          4             4            8
572   142932            7         6         10             5            3
573   183936            3         1          1             1            2
574   324382            1         1          1             1            2
575   378275           10         9          7             3            4
576   385103            5         1          2             1            2
577   690557            5         1          1             1            2
578   695091            1         1          1             1            2
579   695219            1         1          1             1            2
580   824249            1         1          1             1            2
581   871549            5         1          2             1            2
582   878358            5         7         10             6            5
583  1107684            6        10          5             5            4
584  1115762            3         1          1             1            2
585  1217717            5         1          1             6            3
586  1239420            1         1          1             1            2
587  1254538            8        10         10            10            6
588  1261751            5         1          1             1            2
589  1268275            9         8          8             9            6
590  1272166            5         1          1             1            2
591  1294261            4        10          8             5            4
592  1295529            2         5          7             6            4
593  1298484           10         3          4             5            3
594  1311875            5         1          2             1            2
595  1315506            4         8          6             3            4
596  1320141            5         1          1             1            2
597  1325309            4         1          2             1            2
598  1333063            5         1          3             1            2
599  1333495            3         1          1             1            2
600  1334659            5         2          4             1            1
601  1336798            3         1          1             1            2
602  1344449            1         1          1             1            1
603  1350568            4         1          1             1            2
604  1352663            5         4          6             8            4
605   188336            5         3          2             8            5
606   352431           10         5         10             3            5
607   353098            4         1          1             2            2
608   411453            1         1          1             1            2
609   557583            5        10         10            10           10
610   636375            5         1          1             1            2
611   736150           10         4          3            10            3
612   803531            5        10         10            10            5
613   822829            8        10         10            10            6
614  1016634            2         3          1             1            2
615  1031608            2         1          1             1            1
616  1041043            4         1          3             1            2
617  1042252            3         1          1             1            2
619  1061990            4         1          1             1            2
620  1073836            5         1          1             1            2
621  1083817            3         1          1             1            2
622  1096352            6         3          3             3            3
623  1140597            7         1          2             3            2
624  1149548            1         1          1             1            2
625  1174009            5         1          1             2            1
626  1183596            3         1          3             1            3
627  1190386            4         6          6             5            7
628  1190546            2         1          1             1            2
629  1213273            2         1          1             1            2
630  1218982            4         1          1             1            2
631  1225382            6         2          3             1            2
632  1235807            5         1          1             1            2
633  1238777            1         1          1             1            2
634  1253955            8         7          4             4            5
635  1257366            3         1          1             1            2
636  1260659            3         1          4             1            2
637  1268952           10        10          7             8            7
638  1275807            4         2          4             3            2
639  1277792            4         1          1             1            2
640  1277792            5         1          1             3            2
641  1285722            4         1          1             3            2
642  1288608            3         1          1             1            2
643  1290203            3         1          1             1            2
644  1294413            1         1          1             1            2
645  1299596            2         1          1             1            2
646  1303489            3         1          1             1            2
647  1311033            1         2          2             1            2
648  1311108            1         1          1             3            2
649  1315807            5        10         10            10           10
650  1318671            3         1          1             1            2
651  1319609            3         1          1             2            3
652  1323477            1         2          1             3            2
653  1324572            5         1          1             1            2
654  1324681            4         1          1             1            2
655  1325159            3         1          1             1            2
656  1326892            3         1          1             1            2
657  1330361            5         1          1             1            2
658  1333877            5         4          5             1            8
659  1334015            7         8          8             7            3
660  1334667            1         1          1             1            2
661  1339781            1         1          1             1            2
662  1339781            4         1          1             1            2
663 13454352            1         1          3             1            2
664  1345452            1         1          3             1            2
665  1345593            3         1          1             3            2
666  1347749            1         1          1             1            2
667  1347943            5         2          2             2            2
668  1348851            3         1          1             1            2
669  1350319            5         7          4             1            6
670  1350423            5        10         10             8            5
671  1352848            3        10          7             8            5
672  1353092            3         2          1             2            2
673  1354840            2         1          1             1            2
674  1354840            5         3          2             1            3
675  1355260            1         1          1             1            2
676  1365075            4         1          4             1            2
677  1365328            1         1          2             1            2
678  1368267            5         1          1             1            2
679  1368273            1         1          1             1            2
680  1368882            2         1          1             1            2
681  1369821           10        10         10            10            5
682  1371026            5        10         10            10            4
683  1371920            5         1          1             1            2
684   466906            1         1          1             1            2
685   466906            1         1          1             1            2
686   534555            1         1          1             1            2
687   536708            1         1          1             1            2
688   566346            3         1          1             1            2
689   603148            4         1          1             1            2
690   654546            1         1          1             1            2
691   654546            1         1          1             3            2
692   695091            5        10         10             5            4
693   714039            3         1          1             1            2
694   763235            3         1          1             1            2
695   776715            3         1          1             1            3
696   841769            2         1          1             1            2
697   888820            5        10         10             3            7
698   897471            4         8          6             4            3
699   897471            4         8          8             5            4
    Bare.nuclei Bl.cromatin Normal.nucleoli Mitoses     Class
1             1           3               1       1    benign
2            10           3               2       1    benign
3             2           3               1       1    benign
4             4           3               7       1    benign
5             1           3               1       1    benign
6            10           9               7       1 malignant
7            10           3               1       1    benign
8             1           3               1       1    benign
9             1           1               1       5    benign
10            1           2               1       1    benign
11            1           3               1       1    benign
12            1           2               1       1    benign
13            3           4               4       1 malignant
14            3           3               1       1    benign
15            9           5               5       4 malignant
16            1           4               3       1 malignant
17            1           2               1       1    benign
18            1           3               1       1    benign
19           10           4               1       2 malignant
20            1           3               1       1    benign
21           10           5               4       4 malignant
22            7           7              10       1 malignant
23            1           2               1       1    benign
25            1           3               1       1    benign
26            7           3               6       1 malignant
27            1           2               1       1    benign
28            1           2               1       1    benign
29            1           2               1       1    benign
30            1           1               1       1    benign
31            1           2               1       1    benign
32            1           3               1       1    benign
33            5           7               4       3 malignant
34            1           3               1       1    benign
35            1           2               1       1    benign
36            1           2               1       1    benign
37            1           8               9       1 malignant
38            1           7               1       1    benign
39           10           5               6       1 malignant
40            7           7               5       1 malignant
42            3           6               5       2 malignant
43           10           7               3       3 malignant
44            1           3               1       1 malignant
45            1           8              10       1 malignant
46            1           2               1       2    benign
47            9           4               8       1 malignant
48            1           2               1       1    benign
49            1           3               1       1    benign
50            8           3               8       2 malignant
51            3           2               1       5 malignant
52            4           3               4       1 malignant
53            5           4              10       2 malignant
54            8           7               3       7 malignant
55            8           7               1       1 malignant
56            5           3               6       1 malignant
57            6           3               9       1 malignant
58            1           5               4       4 malignant
59           10           5               1       1 malignant
60            2           5               1       1 malignant
61            3           4              10       1 malignant
62            2           2               1       1    benign
63            8           3               3       1 malignant
64            2           3               9       1 malignant
65            1           2               1       1    benign
66            2           4               3      10 malignant
67            1           3               1       1    benign
68           10           4               9       1 malignant
69            9           8               9       8 malignant
70            1           3               2       1    benign
71            1           2               1       1    benign
72            2           7               8      10 malignant
73            1           7               2       1    benign
74           10           4               8       1 malignant
75            4           3               2       3 malignant
76            2           4               2       1    benign
77            1           2               1       1    benign
78            1           2               1       1    benign
79            3           3               1       1    benign
80            1           2               1       1    benign
81            1           7               1       1    benign
82            1           2               1       1    benign
83            1           3               1       1    benign
84            2           7               1       1    benign
85            9           7              10       7 malignant
86            4           4              10      10 malignant
87            8           4               4       1 malignant
88           10           6               8       3 malignant
89            1           3               1       1    benign
90            1           2               1       1    benign
91            1           3               1       1    benign
92            1           1               1       1    benign
93            1           3               1       1    benign
94            1           2               1       1    benign
95            1           3               1       1    benign
96            1           3               1       1    benign
97            1           1               1       1    benign
98            1           3               1       1    benign
99            6           2               9      10 malignant
100          10           7               9       4 malignant
101           5           3              10       2 malignant
102           5           2               5       1 malignant
103           1           3               1       1    benign
104           3           7               1       1 malignant
105           1           8               8       8 malignant
106           3           3               2       7 malignant
107          10           4               1       1 malignant
108          10           5               7       1 malignant
109           1           2               3       1    benign
110           9           7               8       3 malignant
111           2           5               3       2    benign
112           9           3               1       1 malignant
113          10           7               3       3 malignant
114           8           8               1       1 malignant
115           3           3               1       1    benign
116           5           1               1       1    benign
117           2           3               2       1    benign
118          10           7               5       8 malignant
119           3           1               1       1    benign
120           2           3               1       1    benign
121           1           3               1       1    benign
122           2           3               1       1    benign
123          10           5               3       3 malignant
124          10           5               3       1 malignant
125           7           8              10       1 malignant
126           1           2               1       1    benign
127          10           7               5       5 malignant
128           1           3               1       1    benign
129          10           1               6       2 malignant
130           1           1               1       1    benign
131           1           2               1       1    benign
132           1           3               1       1    benign
133          10           3               6       3 malignant
134           1           2               2       1    benign
135           1           2               1       1    benign
136           2           3               3       1    benign
137           1           2               1       1    benign
138           1           1               1       1    benign
139           1           2               1       1    benign
141           1           1               1       1    benign
142           1           1               1       1    benign
143           5           4               3       3 malignant
144           5           1               1       1    benign
145           1           2               1       1    benign
147           8           4               1       1 malignant
148           2           2               1       1    benign
149           1           5               8       1    benign
150          10           7               8       7 malignant
151           1           3               1       1    benign
152          10           5               4       3 malignant
153           5           8              10       1 malignant
154           3           1               1       1    benign
155           1           1               1       1    benign
156          10           3               1       1 malignant
157           1           2               1       1    benign
158           1           3               1       1    benign
160          10           7              10       6 malignant
161          10           5               7       2 malignant
162           1           3               2       1    benign
163           1           3               1       1    benign
164           3           1               1       7    benign
166           2           3               2       1    benign
167          10           3              10       3 malignant
168           1           3               1      10 malignant
169           1           3               1       1    benign
170           1           1               1       1    benign
171           1           1               1       1    benign
172           1           3               1       1    benign
173           1           2               1       1    benign
174          10          10              10       7 malignant
175          10           6               1       1 malignant
176          10           5               7       1 malignant
177           1           3               1       1    benign
178           1           5              10       3 malignant
179           1           3               1       1    benign
180          10           3               1       1 malignant
181           1           3               1       1    benign
182           1           1               1       1    benign
183           1           3               1       1    benign
184          10           7               8       1 malignant
185          10           5               1       1 malignant
186           1           3               1       1    benign
187           8           7              10       1 malignant
188          10           7               7      10 malignant
189           8           9              10       1 malignant
190           1           3               1       1    benign
191           8           7              10       1 malignant
192          10           4              10       3 malignant
193           1           2               1       1    benign
194           1           3               1       1    benign
195           1           3               1       1    benign
196           1           3               1       1    benign
197           7           7               8       2    benign
198           1           3               1       1    benign
199           1           1               1       1    benign
200           1           2               1       1    benign
201          10           7               8       3 malignant
202          10           8               1       1 malignant
203           1           3               1       1    benign
204           1           3               1       1    benign
205           1           3               1       1    benign
206          10           7              10       5 malignant
207           5           3               5       1 malignant
208           1           3               1       1    benign
209           1           3               1       1    benign
210           1           3               1       1    benign
211          10           8              10       6 malignant
212           8           7               7       1 malignant
213           1           3               1       1    benign
214          10           7              10       4 malignant
215          10          10               6       1 malignant
216           5           5              10       2 malignant
217           1           2               1       1    benign
218           1           3               1       1    benign
219           4           8              10       2 malignant
220           1           3               1       1    benign
221           1           3               1       1    benign
222          10           9              10       1 malignant
223           5           2               1       1 malignant
224           8           7               4       1 malignant
225          10           7               9       2 malignant
226           1           2               1       1    benign
227          10           8               9       1 malignant
228           5           7               7       1 malignant
229           1           3               1       1    benign
230          10           9              10       1 malignant
231           7           7               6       1 malignant
232           8           8               9       2 malignant
233           1           4               3       1    benign
234          10           4               1       1 malignant
235           1           3               6       1    benign
237          10           4               8      10 malignant
238           2           4              10       4 malignant
239           9           3              10      10 malignant
240          10           5               3       2 malignant
241           2           2               3       1    benign
242           1           3               1       1    benign
243           1           3               1       1    benign
244           5           5               1       1    benign
245           1           3               1       1    benign
246           2           3               1       1    benign
247          10           7               8       1 malignant
248           9           3               3       1 malignant
249           1           3               6       1    benign
251           1           1               1       1    benign
252          10           5               3       3 malignant
253          10           3               5       3    benign
254          10           7               3       3 malignant
255           8           3               3       1 malignant
256          10           3               6       1 malignant
257           1           1               1       1    benign
258           1           2               1       1    benign
259           1           3               1       1    benign
260           8           3               4       1    benign
261          10           5               1       3 malignant
262          10          10               6       5 malignant
263          10           7               8       1 malignant
264          10           5               5       1 malignant
265           3           5               3       3 malignant
266           1           3               2       1    benign
267          10           4               3       2 malignant
268          10           7               1       1 malignant
269           4           8               7       8 malignant
270           1           3               1       1    benign
271          10           3               9       2 malignant
272           1           3               1       1    benign
273          10           7               1       1 malignant
274           4           3               3       1 malignant
275           1           3               2       1    benign
277           1           2               1       1    benign
278           1           2               1       1    benign
279           1           3               1       1    benign
280           7           3               3       8 malignant
281           1           3               1       1    benign
282           1           3               1       1    benign
283          10           5               6       1 malignant
284          10           5               3       1 malignant
285          10           3               8       2 malignant
286          10          10               7       3 malignant
287          10           4              10      10 malignant
288           1           2               1       1    benign
289           5           5              10       1 malignant
290          10           4              10       4 malignant
291           1           1               1       1    benign
292           1           3               1       1    benign
294          10           2               3       1 malignant
296          10           7               4       1 malignant
297           5           4               7       1    benign
299           1           1               1       1    benign
300          10           7               7       2 malignant
301           4           7              10       1 malignant
302           1           3               1       1    benign
303          10           7              10      10 malignant
304           1           3               1       1    benign
305          10           3               3       1 malignant
306          10           3              10       4 malignant
307           1           3               1       1    benign
308           1           3               1       1    benign
309           3           8               8       4 malignant
310           5           5               1       1    benign
311           1           2               1       1    benign
312           1           1               1       1    benign
313           1           3               5       1 malignant
314           1           1               1       1    benign
315           1           2               1       1    benign
317          10           4               3       1 malignant
318           8           8               9       1 malignant
319           1           3               1       1    benign
320           5           7               3       1    benign
321          10           7               4       6 malignant
323           1           3               1       1    benign
324          10           4               1       1 malignant
325           1           3               1       1    benign
326           1           2               3       1    benign
327          10           5               4       1 malignant
328           1           2               1       1    benign
329           4           3              10       1 malignant
330          10           7               1       1 malignant
331           8           6               1       1 malignant
332           1           3               1       2    benign
333           1           2               2       1    benign
334          10           4               3       1 malignant
335          10           3               4       2 malignant
336           1           1               1       1    benign
337          10           3               4       1 malignant
338           1           3               1       1    benign
339           1           2               1       1    benign
340          10           4               3       1 malignant
341          10           7               6       1 malignant
342           1           3               1       1    benign
343           1           1               1       1    benign
344           1           1               1       1    benign
345          10           9               5       3 malignant
346           1           1               1       1    benign
347           1           1               3       1    benign
348           1           1               3       1    benign
349           1           3               3       1 malignant
350           8           7               6       1 malignant
351           1           1               1       1    benign
352           1           3               1       1    benign
353           3           4               6       1    benign
354          10           4               9       4 malignant
355           1           2               1       1    benign
356           1           2               2       1    benign
357           3           3               3       3 malignant
358          10           7               3       8 malignant
359           4           4              10       3 malignant
360           7           3               5       3 malignant
361          10           8              10      10 malignant
362          10           5               1       4 malignant
363           3           2               1       1    benign
364           3           2               1       1    benign
365           1           3               1       1    benign
366           1           2               1       1    benign
367          10           7              10       7 malignant
368          10           8              10       3 malignant
369           1           1               1       1    benign
370           1           2               1       1    benign
371           1           2               1       1    benign
372           1           1               1       1    benign
373           1           2               1       1    benign
374           1           2               1       1    benign
375           1           2               1       1    benign
376           1           1               1       1    benign
377           1           2               1       1    benign
378           1           2               1       1    benign
379           1           2               2       1    benign
380           1           3               1       1    benign
381           1           1               1       1    benign
382          10           7               8       4 malignant
383           1           3               2       1    benign
384           1           1               1       1    benign
385           1           1               1       1    benign
386           1           1               2       3    benign
387          10           7               1       1 malignant
388           1           3               1       1    benign
389           1           2               2       1    benign
390           2           2               2       1    benign
391           1           2               1       1    benign
392          10           7               9       1 malignant
393           1           2               1       1    benign
394           1           1               1       1    benign
395           1           2               1       1    benign
396           1           2               1       1    benign
397           1           3               1       1    benign
398           1           1               1       1    benign
399           1           2               2       1    benign
400           1           1               1       1    benign
401           9           9               3       8 malignant
402           1           1               1       1    benign
403           1           2               1       1    benign
404           4           1               1       1    benign
405           1           1               2       1    benign
406           1           2               1       1    benign
407           1           2               1       1    benign
408           1           2               1       1    benign
409           2           3               1       1    benign
410           1           2               1       1    benign
411           1           2               1       1    benign
413           4           8               5       1 malignant
414           1           3               1       1    benign
415          10           6               6       1 malignant
416           3           3               5       1    benign
417          10           7               2       1 malignant
418           1           2               1       1    benign
419           2           3               2       2    benign
420           1           1               1       1    benign
421           3           3               1       1    benign
422          10           8               2       1 malignant
423           1           3               3       1    benign
424           1           2               1       1    benign
425           1           1               1       1    benign
426          10          10              10       1 malignant
427           1           1               1       1    benign
428           2           5              10       1 malignant
429           1           2               1       1    benign
430           1           2               1       1    benign
431           1           2               2       1    benign
432           1           3               2       1    benign
433           1           2               2       1    benign
434           1           1               1       1    benign
435           8           4               2       1    benign
436          10           5               1       1 malignant
437           1           2               8       1 malignant
438           1           1               1       1    benign
439           1           1               1       1    benign
440           1           1               1       1    benign
441          10          10               1       1 malignant
442           4           1               1       1    benign
443           3           1               1       1    benign
444           2           1               1       1    benign
445           1           2               1       1    benign
446           1           1               1       1    benign
447           1           1               1       1    benign
448           1           1               1       1    benign
449           1           1               1       1    benign
450          10           8              10       1 malignant
451           1           2               1       1    benign
452           1           1               1       1    benign
453           1           1               1       1    benign
454          10          10               7       1 malignant
455           1           1               1       1    benign
456           6           1               1       2 malignant
457          10           8               6       1 malignant
458           3          10              10       1 malignant
459           1           1               1       1    benign
460           1           1               1       1    benign
461           1           1               1       1    benign
462           5           1               1       1    benign
463           1           1               1       1    benign
464           1           1               2       1    benign
465           1           1               1       1    benign
466           4           7              10       3 malignant
467          10           9               7       1 malignant
468          10           7               6       2 malignant
469           1           1               1       1    benign
470           1           2               1       1    benign
471           1           2               1       1    benign
472           1           1               1       1    benign
473           1           1               1       1    benign
474           1           1               1       1    benign
475           1           1               1       1    benign
476           1           1               1       1    benign
477           1           1               1       1    benign
478           1           1               1       1    benign
479           1           1               1       1    benign
480          10           7               5       1 malignant
481           1           1               1       1    benign
482           1           1               1       1    benign
483           5          10              10      10 malignant
484          10           9              10       1 malignant
485           1           1               1       1    benign
486           3           1               1       1    benign
487           1           2               1       1    benign
488          10           8               1       5 malignant
489           3           3               4       1 malignant
490           4           4               1       1 malignant
491           1           1               1       1    benign
492          10           7               1       1 malignant
493           1           2               1       1    benign
494          10           6               5       2 malignant
495           5           2               1       1    benign
496           1           2               1       1    benign
497           1           1               1       1    benign
498           1           1               1       1    benign
499           1           2               1       1    benign
500           1           2               1       1    benign
501           1           3               1       1    benign
502           1           2               1       1    benign
503           1           2               1       1    benign
504           1           3               1       1    benign
505           1           1               1       1    benign
506           1           1               1       1    benign
507           5           4               8       7 malignant
508           4           1               1       1    benign
509           1           1               1       1    benign
510           1           1               1       1    benign
511           1           1               1       1    benign
512           1           2               1       1    benign
513           1           1               1       1    benign
514           1           2               1       1    benign
515          10           8              10       2 malignant
516          10           9              10       1 malignant
517           1           1               1       1    benign
518           1           2               1       1    benign
519           1           1               1       1    benign
520          10           9               1       1 malignant
521           1           1               1       1    benign
522           1           1               1       1    benign
523           5           7               3       1 malignant
524          10           5               3       1 malignant
525           1           2               1       1    benign
526           1           1               1       1    benign
527           1           1               1       1    benign
528           1           3               1       1    benign
529           1           1               1       1    benign
530           1           2               1       1    benign
531          10           6               9       1 malignant
532           1           2               1       1    benign
533           1           3               1       1    benign
534           1           2               1       1    benign
535           1           2               1       1    benign
536           1           3               1       1    benign
537           1           3               1       1    benign
538           1           3               1       1    benign
539           1           2               1       1    benign
540           1           2               1       1    benign
541           2           2               1       1    benign
542           1           1               1       1    benign
543           1           1               1       1    benign
544           1           2               1       1    benign
545           1           2               1       1    benign
546           1           2               1       1    benign
547          10           7              10       1 malignant
548           1           1               1       1    benign
549           1           1               1       1    benign
550           5           7               8       2 malignant
551           1           2               1       1    benign
552           1           3               1       1    benign
553           1           4               2       1    benign
554           5           2               1       2    benign
555           1           1               1       1    benign
556           1           4               8       1    benign
557           1           2               1       1    benign
558           1           1               1       1    benign
559           1           2               1       1    benign
560           1           2               1       1    benign
561           1           3               1       1    benign
562           1           3               1       1    benign
563           1           3               1       1    benign
564           1           2               1       1    benign
565           1           3               2       1    benign
566          10          10              10       1 malignant
567           1           3               1       1    benign
568           3           2               1       1    benign
569          10           2               5       2 malignant
570           5          10               3       1 malignant
571          10           8               2       1 malignant
572          10           9              10       2 malignant
573           1           2               1       1    benign
574           1           2               1       1    benign
575           2           7               7       1 malignant
576           1           3               1       1    benign
577           1           2               1       1    benign
578           1           2               1       1    benign
579           1           2               1       1    benign
580           1           3               1       1    benign
581           1           2               1       1    benign
582          10           7               5       1 malignant
583          10           6              10       1 malignant
584           1           1               1       1    benign
585           1           1               1       1    benign
586           1           1               1       1    benign
587          10          10              10       1 malignant
588           1           2               2       1    benign
589           3           4               1       1 malignant
590           1           1               1       1    benign
591           1          10               1       1 malignant
592          10           7               6       1 malignant
593          10           4               1       1 malignant
594           1           1               1       1    benign
595          10           7               1       1 malignant
596           1           2               1       1    benign
597           1           2               1       1    benign
598           1           3               1       1    benign
599           1           2               1       1    benign
600           1           1               1       1    benign
601           1           2               1       1    benign
602           1           2               1       1    benign
603           1           2               1       1    benign
604           1           8              10       1 malignant
605          10           8               1       2 malignant
606           8           7               8       3 malignant
607           1           1               1       1    benign
608           1           1               1       1    benign
609          10          10               1       1 malignant
610           1           1               1       1    benign
611          10           7               1       2 malignant
612           2           8               5       1 malignant
613          10          10              10      10 malignant
614           1           2               1       1    benign
615           1           2               1       1    benign
616           1           2               1       1    benign
617           1           2               1       1    benign
619           1           2               1       1    benign
620           1           2               1       1    benign
621           1           2               1       1    benign
622           2           6               1       1    benign
623           1           2               1       1    benign
624           1           1               1       1    benign
625           1           2               1       1    benign
626           4           1               1       1    benign
627           6           7               7       3 malignant
628           5           1               1       1    benign
629           1           1               1       1    benign
630           1           1               1       1    benign
631           1           1               1       1    benign
632           1           2               1       1    benign
633           1           1               1       1    benign
634           3           5              10       1 malignant
635           1           1               1       1    benign
636           1           1               1       1    benign
637           1          10              10       3 malignant
638           2           2               1       1    benign
639           1           1               1       1    benign
640           1           1               1       1    benign
641           1           1               1       1    benign
642           1           2               1       1    benign
643           1           2               1       1    benign
644           1           1               1       1    benign
645           1           1               1       1    benign
646           1           2               1       1    benign
647           1           1               1       1    benign
648           1           1               1       1    benign
649           2          10              10      10 malignant
650           1           2               1       1    benign
651           4           1               1       1    benign
652           1           2               1       1    benign
653           1           2               2       1    benign
654           1           2               1       1    benign
655           1           3               1       1    benign
656           1           2               1       1    benign
657           1           2               1       1    benign
658           1           3               6       1    benign
659          10           7               2       3 malignant
660           1           1               1       1    benign
661           1           2               1       1    benign
662           1           3               1       1    benign
663           1           2               1       1    benign
664           1           2               1       1    benign
665           1           2               1       1    benign
666           1           1               1       1    benign
667           1           1               1       2    benign
668           1           3               1       1    benign
669           1           7              10       3 malignant
670           5           7              10       1 malignant
671           8           7               4       1 malignant
672           1           3               1       1    benign
673           1           3               1       1    benign
674           1           1               1       1    benign
675           1           2               1       1    benign
676           1           1               1       1    benign
677           1           2               1       1    benign
678           1           1               1       1    benign
679           1           1               1       1    benign
680           1           1               1       1    benign
681          10          10              10       7 malignant
682          10           5               6       3 malignant
683           1           3               2       1    benign
684           1           1               1       1    benign
685           1           1               1       1    benign
686           1           1               1       1    benign
687           1           1               1       1    benign
688           1           2               3       1    benign
689           1           1               1       1    benign
690           1           1               1       8    benign
691           1           1               1       1    benign
692           5           4               4       1 malignant
693           1           1               1       1    benign
694           1           2               1       2    benign
695           2           1               1       1    benign
696           1           1               1       1    benign
697           3           8              10       2 malignant
698           4          10               6       1 malignant
699           5          10               4       1 malignant
> 
> 
> str(bc)
'data.frame':   683 obs. of  11 variables:
 $ Id             : chr  "1000025" "1002945" "1015425" "1016277" ...
 $ Cl.thickness   : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 5 5 3 6 4 8 1 2 2 4 ...
 $ Cell.size      : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 1 4 1 8 1 10 1 1 1 2 ...
 $ Cell.shape     : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 1 4 1 8 1 10 1 2 1 1 ...
 $ Marg.adhesion  : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 1 5 1 1 3 8 1 1 1 1 ...
 $ Epith.c.size   : Ord.factor w/ 10 levels "1"<"2"<"3"<"4"<..: 2 7 2 3 2 7 2 2 2 2 ...
 $ Bare.nuclei    : Factor w/ 10 levels "1","2","3","4",..: 1 10 2 4 1 10 10 1 1 1 ...
 $ Bl.cromatin    : Factor w/ 10 levels "1","2","3","4",..: 3 3 3 3 3 9 3 3 1 2 ...
 $ Normal.nucleoli: Factor w/ 10 levels "1","2","3","4",..: 1 2 1 7 1 7 1 1 1 1 ...
 $ Mitoses        : Factor w/ 9 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 5 1 ...
 $ Class          : Factor w/ 2 levels "benign","malignant": 1 1 1 1 1 2 1 1 1 1 ...
> 
> 
> glm(Class ~ Cell.shape, family="binomial", data = bc)

Call:  glm(formula = Class ~ Cell.shape, family = "binomial", data = bc)

Coefficients:
 (Intercept)  Cell.shape.L  Cell.shape.Q  Cell.shape.C  Cell.shape^4  
       4.189        20.911         6.848         5.763        -1.267  
Cell.shape^5  Cell.shape^6  Cell.shape^7  Cell.shape^8  Cell.shape^9  
      -4.439        -5.183        -3.013        -1.289        -0.860  

Degrees of Freedom: 682 Total (i.e. Null);  673 Residual
Null Deviance:      884.4 
Residual Deviance: 243.6        AIC: 263.6
> 
> 
> bc <- bc[, -1]
> 
> 
> for(i in 1:9){
+  bc[, i] <- as.numeric(as.character(bc[, i]))
+ }
> 
> 
> 
> bc$Class <- ifelse(bc$Class == "malignant", 1, 0)
> bc$Class <- factor(bc$Class, levels = c(0, 1))
> 
> table(bc$Class)

  0   1 
444 239 
> library(caret)
Error in library(caret) : there is no package called ‘caret’
> utils:::menuInstallPkgs()
also installing the dependencies ‘numDeriv’, ‘SQUAREM’, ‘abind’, ‘lava’, ‘kernlab’, ‘CVST’, ‘DEoptimR’, ‘magic’, ‘prodlim’, ‘DRR’, ‘robustbase’, ‘sfsmisc’, ‘geometry’, ‘iterators’, ‘ipred’, ‘dimRed’, ‘timeDate’, ‘ddalpha’, ‘gower’, ‘RcppRoll’, ‘foreach’, ‘ModelMetrics’, ‘recipes’

trying URL 'https://cloud.r-project.org/bin/windows/contrib/3.4/numDeriv_2016.8-1.zip'
Content type 'application/zip' length 115375 bytes (112 KB)
downloaded 112 KB

trying URL 'https://cloud.r-project.org/bin/windows/contrib/3.4/SQUAREM_2017.10-1.zip'
Content type 'application/zip' length 272523 bytes (266 KB)
downloaded 266 KB

trying URL 'https://cloud.r-project.org/bin/windows/contrib/3.4/abind_1.4-5.zip'
Content type 'application/zip' length 40443 bytes (39 KB)
downloaded 39 KB

trying URL 'https://cloud.r-project.org/bin/windows/contrib/3.4/lava_1.6.1.zip'
Content type 'application/zip' length 2235249 bytes (2.1 MB)
downloaded 2.1 MB

trying URL 'https://cloud.r-project.org/bin/windows/contrib/3.4/kernlab_0.9-26.zip'
Content type 'application/zip' length 2248630 bytes (2.1 MB)
downloaded 2.1 MB

trying URL 'https://cloud.r-project.org/bin/windows/contrib/3.4/CVST_0.2-2.zip'
Content type 'application/zip' length 57849 bytes (56 KB)
downloaded 56 KB

trying URL 'https://cloud.r-project.org/bin/windows/contrib/3.4/DEoptimR_1.0-8.zip'
Content type 'application/zip' length 41956 bytes (40 KB)
downloaded 40 KB

trying URL 'https://cloud.r-project.org/bin/windows/contrib/3.4/magic_1.5-8.zip'
Content type 'application/zip' length 505395 bytes (493 KB)
downloaded 493 KB

trying URL 'https://cloud.r-project.org/bin/windows/contrib/3.4/prodlim_2018.04.18.zip'
Content type 'application/zip' length 238791 bytes (233 KB)
downloaded 233 KB

trying URL 'https://cloud.r-project.org/bin/windows/contrib/3.4/DRR_0.0.3.zip'
Content type 'application/zip' length 148548 bytes (145 KB)
downloaded 145 KB

trying URL 'https://cloud.r-project.org/bin/windows/contrib/3.4/robustbase_0.93-0.zip'
Content type 'application/zip' length 3220157 bytes (3.1 MB)
downloaded 3.1 MB

trying URL 'https://cloud.r-project.org/bin/windows/contrib/3.4/sfsmisc_1.1-2.zip'
Content type 'application/zip' length 581375 bytes (567 KB)
downloaded 567 KB

trying URL 'https://cloud.r-project.org/bin/windows/contrib/3.4/geometry_0.3-6.zip'
Content type 'application/zip' length 684136 bytes (668 KB)
downloaded 668 KB

trying URL 'https://cloud.r-project.org/bin/windows/contrib/3.4/iterators_1.0.9.zip'
Content type 'application/zip' length 320519 bytes (313 KB)
downloaded 313 KB

trying URL 'https://cloud.r-project.org/bin/windows/contrib/3.4/ipred_0.9-6.zip'
Content type 'application/zip' length 298278 bytes (291 KB)
downloaded 291 KB

trying URL 'https://cloud.r-project.org/bin/windows/contrib/3.4/dimRed_0.1.0.zip'
Content type 'application/zip' length 331591 bytes (323 KB)
downloaded 323 KB

trying URL 'https://cloud.r-project.org/bin/windows/contrib/3.4/timeDate_3043.102.zip'
Content type 'application/zip' length 805975 bytes (787 KB)
downloaded 787 KB

trying URL 'https://cloud.r-project.org/bin/windows/contrib/3.4/ddalpha_1.3.3.zip'
Content type 'application/zip' length 1540099 bytes (1.5 MB)
downloaded 1.5 MB

trying URL 'https://cloud.r-project.org/bin/windows/contrib/3.4/gower_0.1.2.zip'
Content type 'application/zip' length 122826 bytes (119 KB)
downloaded 119 KB

trying URL 'https://cloud.r-project.org/bin/windows/contrib/3.4/RcppRoll_0.2.2.zip'
Content type 'application/zip' length 631804 bytes (616 KB)
downloaded 616 KB

trying URL 'https://cloud.r-project.org/bin/windows/contrib/3.4/foreach_1.4.4.zip'
Content type 'application/zip' length 388495 bytes (379 KB)
downloaded 379 KB

trying URL 'https://cloud.r-project.org/bin/windows/contrib/3.4/ModelMetrics_1.1.0.zip'
Content type 'application/zip' length 592798 bytes (578 KB)
downloaded 578 KB

trying URL 'https://cloud.r-project.org/bin/windows/contrib/3.4/recipes_0.1.2.zip'
Content type 'application/zip' length 1015259 bytes (991 KB)
downloaded 991 KB

trying URL 'https://cloud.r-project.org/bin/windows/contrib/3.4/caret_6.0-80.zip'
Content type 'application/zip' length 5374595 bytes (5.1 MB)
downloaded 5.1 MB

package ‘numDeriv’ successfully unpacked and MD5 sums checked
package ‘SQUAREM’ successfully unpacked and MD5 sums checked
package ‘abind’ successfully unpacked and MD5 sums checked
package ‘lava’ successfully unpacked and MD5 sums checked
package ‘kernlab’ successfully unpacked and MD5 sums checked
package ‘CVST’ successfully unpacked and MD5 sums checked
package ‘DEoptimR’ successfully unpacked and MD5 sums checked
package ‘magic’ successfully unpacked and MD5 sums checked
package ‘prodlim’ successfully unpacked and MD5 sums checked
package ‘DRR’ successfully unpacked and MD5 sums checked
package ‘robustbase’ successfully unpacked and MD5 sums checked
package ‘sfsmisc’ successfully unpacked and MD5 sums checked
package ‘geometry’ successfully unpacked and MD5 sums checked
package ‘iterators’ successfully unpacked and MD5 sums checked
package ‘ipred’ successfully unpacked and MD5 sums checked
package ‘dimRed’ successfully unpacked and MD5 sums checked
package ‘timeDate’ successfully unpacked and MD5 sums checked
package ‘ddalpha’ successfully unpacked and MD5 sums checked
package ‘gower’ successfully unpacked and MD5 sums checked
package ‘RcppRoll’ successfully unpacked and MD5 sums checked
package ‘foreach’ successfully unpacked and MD5 sums checked
package ‘ModelMetrics’ successfully unpacked and MD5 sums checked
package ‘recipes’ successfully unpacked and MD5 sums checked
package ‘caret’ successfully unpacked and MD5 sums checked

The downloaded binary packages are in
        C:\Users\xxxxx\AppData\Local\Temp\RtmpMXDUgl\downloaded_packages
> '%ni%' <- Negate('%in%')
> options(scipen=999)
> 
> 
> set.seed(100)
> 
> trainDataIndex <- createDataPartition(bc$Class, p=0.7, list=F)
Error in createDataPartition(bc$Class, p = 0.7, list = F) : 
  could not find function "createDataPartition"
> trainDataIndex <- createDataPartition(bc$Class, p=0.7, list=F)
Error in createDataPartition(bc$Class, p = 0.7, list = F) : 
  could not find function "createDataPartition"
> library(caret)
Loading required package: lattice
Loading required package: ggplot2
Warning messages:
1: package ‘caret’ was built under R version 3.4.4 
2: package ‘ggplot2’ was built under R version 3.4.2 
> trainDataIndex <- createDataPartition(bc$Class, p=0.7, list=F)
> trainData <- bc[trainDataIndex, ]
> testData <- bc[-trainDataIndex, ]
> table(trainData$Class)

  0   1 
311 168 
> set.seed(100)
> 
> down_train <- downSample(x = trainData[, colnames(trainData) %ni% "Class"], y = trainData$Class)
> 
> trable(down_train$Class)
Error in trable(down_train$Class) : could not find function "trable"
> table(down_train$Class)

  0   1 
168 168 
> 
> 
> 
> set.seed(100)
> up_train <- upSample(x = trainData[, colnames(trainData) %ni% "Class"], y = trainData$Class)
> 
> table(up_train$Class)

  0   1 
311 311 
> 
> 
> 
> 
> logitmod <- glm(Class ~ Cl.thickness + Cell.size + Cell.shape, family = "binomial", data=down_train)
> 
> 
> summary(logitmod)

Call:
glm(formula = Class ~ Cl.thickness + Cell.size + Cell.shape, 
    family = "binomial", data = down_train)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-3.8054  -0.2008  -0.0597   0.1123   2.6206  

Coefficients:
             Estimate Std. Error z value            Pr(>|z|)    
(Intercept)   -6.7500     0.8476  -7.964 0.00000000000000167 ***
Cl.thickness   0.4932     0.1205   4.094 0.00004241566655254 ***
Cell.size      0.6982     0.2309   3.024             0.00250 ** 
Cell.shape     0.6782     0.2268   2.991             0.00278 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 465.79  on 335  degrees of freedom
Residual deviance: 101.03  on 332  degrees of freedom
AIC: 109.03

Number of Fisher Scoring iterations: 7

> pred <- predict(logitmod, newdata = testData, type = "response")
> 
> 
> y_pred_num <- ifelse(pred > 0.5, 1, 0)
> 
> y_pred <- factor(y_pred_num, levels=c(0,1))
> y_act <- testData$Class
> 
> mean(y_pred == y_act)
[1] 0.9558824
> 
