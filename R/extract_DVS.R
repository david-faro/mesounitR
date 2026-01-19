#' Prepare Depth, Velocity, Substrate (DVS) Table for MesoHABSIM
#'
#' @param poly_mesohabitats sf object of polygons representing mesohabitats
#' @param flow sf object of mesh elements with flow data (DEPTH in m, VEL in m/s)
#' @param depth_min numeric, minimum DEPTH to filter wet channel elements
#' @param ras_substrate stars object of substrate raster (optional). Values set to NA if no substrate provided
#' @param sub_codes data.frame with columns `class_id` and `type` for substrate classification (optional)
#' @param file_name character string, path and file name to save output table. Default: 'mesohabitats_DVS.txt'
#' @param nmax numeric, maximum number of points per HMU for subsampling (default 50)
#'
#' @export
extract_DVS <- function(poly_mesohabitats,
                        flow,
                        depth_min,
                        ras_substrate=NULL,
                        sub_codes=NULL,
                        file_name='mesohabitats_DVS.txt',
                        nmax=50) {

  # --- 1. Check 'flow' attributes ---
  if (!all(c("DEPTH", "VEL") %in% names(flow))) {
    stop("In 'flow' attributes 'DEPTH' and/or 'VEL' are missing.", call. = FALSE)
  }

  # check that depth_min is numeric and > 0
  if (!is.numeric(depth_min) || length(depth_min) != 1L || is.na(depth_min) || depth_min <= 0) {
    stop("`depth_min` must be a single numeric value > 0.", call. = FALSE)
  }

  # --- 2. Substrate inputs are optional ---
  use_substrate <- !is.null(ras_substrate) && !is.null(sub_codes)

  # If only one is provided, ignore substrate and warn (prevents hard-to-debug partial inputs)
  if (xor(is.null(ras_substrate), is.null(sub_codes))) {
    warning("Only one of `ras_substrate` / `sub_codes` was provided; `SUBSTRATE` will be set to NA.", call. = FALSE)
    use_substrate <- FALSE
  }

  if (use_substrate) {
    # --- 2a. Check 'sub_codes' structure and types ---
    required_cols <- c("class_id", "type")
    missing_cols <- setdiff(required_cols, names(sub_codes))
    if (length(missing_cols) > 0) {
      stop("In 'sub_codes', required columns 'class_id' and 'type' are missing.", call. = FALSE)
    }

    if (!is.numeric(sub_codes$class_id)) {
      stop("In 'sub_codes', column 'class_id' must be of type numeric.", call. = FALSE)
    }
    if (!is.character(sub_codes$type)) {
      stop("In 'sub_codes', column 'type' must be of type character.", call. = FALSE)
    }

    # --- 3. Check that raster substrate values match sub_codes$class_id ---
    ras_vals <- unique(as.numeric(ras_substrate[[1]]))
    ras_vals <- ras_vals[!is.na(ras_vals)]

    if (!setequal(ras_vals, sub_codes$class_id)) {
      warning("Unique values in 'ras_substrate' do not match 'sub_codes$class_id'.", call. = FALSE)
    }
  }

  # --- Continue with main function body ---

  # define breaks, in m (depth) or m/s (velocity), according to MesoHABSIM
  # last value (100) is only added as an unrealistic maximum value for the last category (>1.2)
  brks <- c(0,0.15,0.3,0.45,0.6,0.75,0.9,1.05,1.2,100) #

  n <- nrow(poly_mesohabitats)

  # create empty data.frame
  table.dvs <- data.frame(HMU_NUM = numeric(),
                          HMU_TYPE = character(),
                          PTNUM = numeric(),
                          DEPTH = numeric(),
                          VELOCITY = numeric(),
                          SUBSTRATE = character())

  # convert flow elements into points (centroids)

  # extract only wet polygons
  flow_wet <- flow %>%
    filter(DEPTH >= depth_min)

  flow_pts <- sf::st_centroid(flow_wet)

  for (i in 1:n) {

    # flow pts
    pts_contained <- as.matrix(st_contains(poly_mesohabitats[i,],flow_pts))
    flow_in_habitat <- flow_pts[pts_contained,]

    x.depth <- flow_in_habitat$DEPTH
    x.vel <- flow_in_habitat$VEL

    # remove NAs
    x.depth <- x.depth[!is.na(x.depth)]  # remove all NA values
    x.vel <- x.vel[!is.na(x.vel)]  # remove all NA values

    # substrate: either extract, or set to NA
    if (use_substrate) {
      sub_in_habitat <- stars::st_extract(ras_substrate, flow_in_habitat)
      names(sub_in_habitat)[1] <- "sub_id"
      sub_in_habitat <- sub_in_habitat[!is.na(sub_in_habitat$sub_id), ]

      x.sub <- convert_substrate(sub_in_habitat$sub_id, sub_codes)
    } else {
      x.sub <- rep(NA_character_, min(length(x.depth), length(x.vel)))
    }

    # n points
    n.pts_depth <- length(x.depth)
    n.pts_vel <- length(x.vel)
    n.pts_sub <- length(x.sub)

    n.pts_min <- min(n.pts_depth,n.pts_vel,n.pts_sub)
    n.pts_max <- max(n.pts_depth,n.pts_vel,n.pts_sub)

    # if n.pts larger than nmax, subsample to nmax
    # else, if n.pts for depth, vel or sub unequal, subsample to smallest n.pts
    if (n.pts_min > nmax) {

      x.depth <- subsample_DV_to_hist(x.depth,brks,nmax)
      x.vel <- subsample_DV_to_hist(x.vel,brks,nmax)

      if (use_substrate) {
        x.sub <- subsample_sub_to_hist(x.sub, nmax)
      } else {
        x.sub <- rep(NA_character_, nmax)
      }

    } else if (n.pts_max > n.pts_min) {

      x.depth <- subsample_DV_to_hist(x.depth,brks,n.pts_min)
      x.vel <- subsample_DV_to_hist(x.vel,brks,n.pts_min)

      if (use_substrate) {
        x.sub <- subsample_sub_to_hist(x.sub, n.pts_min)
      } else {
        x.sub <- rep(NA_character_, n.pts_min)
      }

    }

    hmu.dvs <- data.frame(HMU_NUM = poly_mesohabitats$HMU_NUM[i],
                          HMU_TYPE = poly_mesohabitats$HMU_TYPE[i],
                          PNTNUM = 1:length(x.sub),
                          DEPTH = x.depth,
                          VELOCITY = x.vel,
                          SUBSTRATE = x.sub)

    table.dvs <- rbind(table.dvs, hmu.dvs)

  }

  # drop rows with NA values
  table.dvs <- na.omit(table.dvs)

  write.table(table.dvs, file_name, sep="\t", col.names = T, row.names = F, quote = F)

  invisible(table.dvs)  # return the table invisibly
}


# --------------------------
# Internal helper functions
# --------------------------

# Convert substrate IDs to names
convert_substrate <- function(sub_ids, sub_codes) {
  sub_class <- character(length(sub_ids))
  for (j in seq_len(nrow(sub_codes))) {
    sub_class[sub_ids == sub_codes$class_id[j]] <- sub_codes$type[j]
  }
  return(sub_class)
}

# Subsample depth and velocity vectors (to nmax) maintaining histogram proportions
subsample_DV_to_hist <- function(x,brks,nmax) {

  # function subsample a population mantainining same proportions as histogram defined by given breaks
  # x = numeric vector with population to sub-sample
  # brks = breaks used in histogram function
  # nmax = length of sub-sampled population

  if (!(brks[length(brks)] > max(x))) {
    brks[length(brks)] <- max(x)
  }

  xhist <- hist(x, breaks=brks, plot=F)

  xs <- numeric()

  prop <- smart.round(xhist$counts/sum(xhist$counts)*nmax)

  for (i in 1:(length(prop))) {

    sample_in_brk <- x[x>=brks[i] & x<brks[i+1]]

    sub <- sample(sample_in_brk, prop[i], replace = F)

    xs <- c(xs,sub)

  }

  return(xs)
}

# Subsample substrates (to nmax) maintaining histogram proportions
subsample_sub_to_hist <- function(x,nmax) {

  # function subsample a population mantainining same proportions as histogram defined by given breaks
  # x = categorical vector with population to sub-sample
  # nmax = length of sub-sampled population

  x <- x[!is.na(x)] # delete NA values

  xs <- numeric()

  x <- factor(x)
  lev <- levels(x)

  prop <- smart.round(table(x)/length(x)*nmax)

  for (i in 1:(length(prop))) {

    sub <- rep(lev[i],prop[i])

    xs <- c(xs,sub)

  }

  return(xs)
}

# smart rounding keeping sum constant
smart.round <- function(x) {
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y
}

