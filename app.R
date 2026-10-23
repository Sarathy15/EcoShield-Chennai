# =====================================================================
# EcoShield: Chennai Environmental Intelligence Dashboard
# Save as app.R in the project root. Put datasets in ./data/.
# Expected files:
#   rainfall_preliminary.csv OR Chennai_Historical_Rainfall_Dataset.csv
#   Chennai_Air_Quality_Index.csv
#   chennai parks and location dataset.csv
#   Chennai_Flood_Inundation_Dataset.kml
#
# Data limitations are intentionally displayed: rainfall units are unverified,
# rainfall conflicts are excluded, AQI year is unknown, and KML points are not
# live flood alerts.
#
# THIS VERSION: Overview, Historical Rainfall and Flood Map pages were upgraded.
# Parks page unchanged; Air Quality page upgraded; the former Action Plan page is now the Live Environmental Monitor (nav value kept as "Action Plan").
#
# Optional packages (features switch off gracefully if missing): sf, changepoint, httr2 + xml2 (Live Monitor page)
#   sf          -> reads the flood KML (Flood Map + Overview map)
#   changepoint -> optional change-point detection (Rainfall page)
# =====================================================================

required <- c("shiny","bslib","dplyr","tidyr","readr","stringr","lubridate","ggplot2","plotly","DT","leaflet","htmltools","scales","jsonlite")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly=TRUE)]
if(length(missing)) stop("Install missing packages with: install.packages(c(", paste(sprintf('"%s"', missing), collapse=", "), "))", call.=FALSE)

library(shiny); library(bslib); library(dplyr); library(tidyr); library(readr); library(stringr)
library(lubridate); library(ggplot2); library(plotly); library(DT); library(leaflet); library(htmltools); library(scales)
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

have_sf <- requireNamespace("sf", quietly=TRUE)
have_cp <- requireNamespace("changepoint", quietly=TRUE)

# ---- Analysis constants (heuristics, documented in the Methods panel) ----
LOW_N         <- 30L   # stations with fewer records are flagged "low coverage" (heuristic, not a statistical rule)
OV_MIN_N      <- 5L    # minimum records for a month to be reported as "highest month" on the Overview
MIN_REF_YEARS <- 10L   # minimum reference years (per calendar month) required for anomaly baselines
APP_LOADED    <- Sys.time()

find_file <- function(candidates) {
  p <- file.path("data", candidates); hit <- p[file.exists(p)]
  if(length(hit)) return(hit[1])
  hit <- candidates[file.exists(candidates)]
  if(length(hit)) return(hit[1])
  NA_character_
}
read_csv_safe <- function(path) {
  if(is.na(path) || !file.exists(path)) return(NULL)
  tryCatch(read_csv(path, show_col_types=FALSE, name_repair="unique"), error=function(e) { warning(conditionMessage(e)); NULL })
}

rain_path <- find_file(c("rainfall_preliminary.csv","Chennai_Historical_Rainfall_Dataset.csv"))
aq_path <- find_file(c("Chennai_Air_Quality_Index.csv","Chennai_Air_Quality_Index (1).csv"))
parks_path <- find_file(c("chennai parks and location dataset.csv","chennai_parks_and_location_dataset.csv","Chennai_Parks_Dataset.csv"))
kml_path <- find_file(c("Chennai_Flood_Inundation_Dataset.kml","Chennai_Flood_Inundation_Dataset (1).kml"))
rain_raw <- read_csv_safe(rain_path); aq_raw <- read_csv_safe(aq_path); parks_raw <- read_csv_safe(parks_path)

# ---------------------------------------------------------------------
# Rainfall cleaning (UNCHANGED RULES): remove exact duplicates; exclude
# station/date groups with conflicting values. Counts at each stage are
# now recorded in `rain_audit` so the dashboard can report exclusions.
# ---------------------------------------------------------------------
rain <- tibble(); rain_conflicts <- tibble()
rain_audit <- list(raw=0L, invalid=0L, bad_date=0L, bad_value=0L, bad_station=0L, dups=0L,
                   conflict_rows=0L, conflict_groups=0L, final=0L, zeros=0L)
if(!is.null(rain_raw)) {
  names(rain_raw) <- make.names(names(rain_raw), unique=TRUE)
  if(all(c("District","Station","Rainfall","Date") %in% names(rain_raw))) {
    rain_typed <- rain_raw |> transmute(District=str_squish(as.character(District)), Station=str_squish(as.character(Station)),
                                        Rainfall=suppressWarnings(as.numeric(Rainfall)), Date=as.Date(parse_date_time(as.character(Date), orders=c("dmy","ymd","mdy","d-b-Y","d-B-Y"), quiet=TRUE)))
    rain_valid <- rain_typed |> filter(!is.na(Station), Station!="", !is.na(Date), is.finite(Rainfall))
    rain <- rain_valid |> distinct()
    conflicts <- rain |> group_by(District,Station,Date) |> summarise(n_values=n_distinct(Rainfall),.groups="drop") |> filter(n_values>1)
    rain_conflicts <- rain |> inner_join(conflicts,by=c("District","Station","Date")) |> arrange(Station,Date)
    rain <- rain |> anti_join(conflicts,by=c("District","Station","Date")) |> distinct(District,Station,Date,Rainfall,.keep_all=TRUE)
    rain_audit <- list(raw=nrow(rain_typed), invalid=nrow(rain_typed)-nrow(rain_valid),
                       bad_date=sum(is.na(rain_typed$Date)), bad_value=sum(!is.finite(rain_typed$Rainfall)),
                       bad_station=sum(is.na(rain_typed$Station) | rain_typed$Station==""),
                       dups=nrow(rain_valid)-nrow(distinct(rain_valid)),
                       conflict_rows=nrow(rain_conflicts), conflict_groups=nrow(conflicts),
                       final=nrow(rain), zeros=sum(rain$Rainfall==0))
  }
}

# AQI: no year is inferred; numeric AQI values only used for descriptive comparisons.
aq <- tibble()
if(!is.null(aq_raw)) {
  names(aq_raw) <- make.names(names(aq_raw), unique=TRUE)
  if(all(c("Station","AQI.Value") %in% names(aq_raw))) {
    # Add optional columns before mutate(); names(.) is not available inside mutate().
    if(!"AQI.Index" %in% names(aq_raw)) aq_raw[["AQI.Index"]] <- NA_character_
    if(!"Prominent.Pollutant" %in% names(aq_raw)) aq_raw[["Prominent.Pollutant"]] <- NA_character_
    aq <- aq_raw |>
      mutate(
        Station = str_squish(as.character(Station)),
        AQI.Value = suppressWarnings(as.numeric(AQI.Value)),
        AQI.Index = str_squish(as.character(AQI.Index)),
        Prominent.Pollutant = str_squish(as.character(Prominent.Pollutant))
      ) |>
      filter(!is.na(Station), Station != "")
  }
}

# ---------------------------------------------------------------------
# AIR QUALITY PAGE DATA MODEL (additive).  `aq` above is unchanged and still
# feeds the Overview page.  Everything below only prepares the Air Quality page.
# No AQI category thresholds are assumed: the file does not state which AQI
# standard it follows, so categories are shown exactly as supplied.
# ---------------------------------------------------------------------
AQ_MIN_N <- 5L   # minimum records per station before "most variable station" is reported (heuristic)
AQ_EMPTY <- "No records match the current filters. Adjust the filters or press Reset filters."
AQ_INVALID_TOKENS <- c("","-","--","_","na","n/a","nan","null","none","n/a (pc issue)")
aq_bad_label <- function(x) { x <- tolower(str_squish(as.character(x))); is.na(x) | x %in% AQ_INVALID_TOKENS }
aq_fmt <- function(v) ifelse(is.finite(v), ifelse(v==round(v), formatC(v,format="f",digits=0,big.mark=","), formatC(v,format="f",digits=1,big.mark=",")), "N/A")
aq_station_stats <- function(x) x |> group_by(Station) |>
  summarise(N=n(), Mean=mean(AQI.Value), Median=median(AQI.Value), SD=if(n()>1) sd(AQI.Value) else NA_real_,
            Min=min(AQI.Value), Max=max(AQI.Value), .groups="drop")
aq_tip <- function(s) paste0("<b>",htmltools::htmlEscape(s$Station),"</b><br>Mean: ",aq_fmt(round(s$Mean,1)),"<br>Median: ",aq_fmt(round(s$Median,1)),
                             "<br>Observations: ",s$N,"<br>Minimum: ",aq_fmt(round(s$Min,1)),"<br>Maximum: ",aq_fmt(round(s$Max,1)))

aq_x <- tibble(Station=character(), AQI.Value=numeric(), AQI.Index=character(), Prominent.Pollutant=character(),
               AQI.Cat=character(), AQI.Poll=character())
aq_poll_cols <- tibble(name=character(), label=character(), unit=character())
aq_geo <- tibble()
aq_dq <- list(raw_rows=0L, kept_rows=0L, no_station=0L, aqi_missing=0L, aqi_nonnumeric=0L, aqi_negative=0L, aqi_over500=0L,
              dup_rows=0L, bad_cat=0L, bad_poll=0L, date_cols=character(), station_variants=0L,
              cat_labels=character(), poll_labels=character(), poll_neg=0L, geo_cols=FALSE)
if(nrow(aq)>0 && !is.null(aq_raw)) {
  st_raw <- str_squish(as.character(aq_raw$Station)); keep <- !is.na(st_raw) & st_raw != ""
  aqi_chr <- str_squish(as.character(aq_raw[["AQI.Value"]]))[keep]
  aqi_num <- suppressWarnings(as.numeric(aqi_chr))
  # Original header text (units are only trusted when written in a column name, e.g. "PM2.5 (ug/m3)")
  orig_names <- tryCatch(names(read_csv(aq_path, n_max=0, show_col_types=FALSE, name_repair="minimal")), error=function(e) character())
  orig_full <- names(aq_raw); if(length(orig_names) && length(orig_names)<=ncol(aq_raw)) orig_full[seq_along(orig_names)] <- orig_names
  aq_x <- aq |> mutate(AQI.Cat=ifelse(aq_bad_label(AQI.Index),NA_character_,AQI.Index),
                       AQI.Poll=ifelse(aq_bad_label(Prominent.Pollutant),NA_character_,Prominent.Pollutant))
  # Numeric pollutant concentration columns (detected from column names; validated numerically)
  nm_low <- tolower(names(aq_raw))
  cand <- which(grepl("(^|[._])(pm2\\.?5|pm10|no2|so2|nh3|co|o3|pb)([._]|$)", nm_low) &
                  !names(aq_raw) %in% c("Station","AQI.Value","AQI.Index","Prominent.Pollutant"))
  poll_neg <- 0L
  for(i in cand) {
    v <- suppressWarnings(as.numeric(as.character(aq_raw[[i]])))[keep]
    if(length(v)!=nrow(aq_x) || mean(is.finite(v))<0.5 || sum(is.finite(v))<3) next
    poll_neg <- poll_neg + sum(v<0,na.rm=TRUE); v[!is.finite(v) | v<0] <- NA_real_
    unit <- str_match(orig_full[i],"\\(([^)]+)\\)")[,2]
    lab <- str_squish(str_replace(orig_full[i],"\\([^)]*\\)",""))
    nm <- if(is.na(unit)) lab else paste0(lab," (",unit,")")
    if(nm %in% names(aq_x)) next
    aq_x[[nm]] <- v
    aq_poll_cols <- bind_rows(aq_poll_cols, tibble(name=nm, label=lab, unit=ifelse(is.na(unit),"unit not stated in source",unit)))
  }
  if(nrow(aq_poll_cols)>6) aq_poll_cols <- aq_poll_cols[1:6,]
  # Coordinates only if the file really has latitude/longitude columns
  la <- which(nm_low %in% c("latitude","lat")); lo <- which(nm_low %in% c("longitude","lon","long","lng"))
  if(length(la) && length(lo)) {
    g <- tibble(Station=st_raw[keep], Latitude=suppressWarnings(as.numeric(as.character(aq_raw[[la[1]]])))[keep],
                Longitude=suppressWarnings(as.numeric(as.character(aq_raw[[lo[1]]])))[keep]) |>
      filter(is.finite(Latitude),is.finite(Longitude),between(Latitude,-90,90),between(Longitude,-180,180))
    if(nrow(g)) aq_geo <- g |> group_by(Station) |> summarise(Latitude=median(Latitude),Longitude=median(Longitude),.groups="drop")
  }
  norm_st <- tolower(str_replace_all(aq$Station,"[^A-Za-z0-9]",""))
  aq_dq <- list(raw_rows=nrow(aq_raw), kept_rows=nrow(aq), no_station=sum(!keep),
                aqi_missing=sum(is.na(aqi_chr) | aqi_chr==""), aqi_nonnumeric=sum(!is.na(aqi_chr) & aqi_chr!="" & is.na(aqi_num)),
                aqi_negative=sum(aqi_num<0,na.rm=TRUE), aqi_over500=sum(aqi_num>500,na.rm=TRUE),
                dup_rows=sum(duplicated(aq_raw)), bad_cat=sum(aq_bad_label(aq$AQI.Index) & is.finite(aq$AQI.Value)),
                bad_poll=sum(aq_bad_label(aq$Prominent.Pollutant) & is.finite(aq$AQI.Value)),
                date_cols=names(aq_raw)[grepl("date|year|time|month|day",nm_low)],
                station_variants=n_distinct(aq$Station)-n_distinct(norm_st),
                cat_labels=sort(unique(na.omit(aq_x$AQI.Cat))), poll_labels=sort(unique(na.omit(aq_x$AQI.Poll))),
                poll_neg=poll_neg, geo_cols=nrow(aq_geo)>0)
}
aq_choices <- list(station=sort(unique(aq_x$Station)), poll=sort(unique(na.omit(aq_x$AQI.Poll))), cat=sort(unique(na.omit(aq_x$AQI.Cat))))
aq_rng <- if(any(is.finite(aq_x$AQI.Value))) c(floor(min(aq_x$AQI.Value,na.rm=TRUE)),ceiling(max(aq_x$AQI.Value,na.rm=TRUE))) else c(0,500)
aq_n_st <- max(1L, n_distinct(aq_x$Station))
aq_rank_h <- paste0(max(300, 26*aq_n_st+110),"px"); aq_box_h <- paste0(max(360, 28*aq_n_st+120),"px")

# Park column names vary in punctuation; locate them robustly.
parks <- tibble()
if(!is.null(parks_raw)) {
  nms <- names(parks_raw); zl <- which(tolower(nms)=="zone"); wl <- which(tolower(nms)=="ward")
  pl <- which(str_detect(tolower(nms),"name.*location.*park|park.*name"))
  if(length(zl)&&length(wl)&&length(pl)) parks <- parks_raw |> transmute(Zone=str_squish(as.character(.data[[nms[zl[1]]]])),
                                                                         Ward=str_squish(as.character(.data[[nms[wl[1]]]])), Park=str_squish(as.character(.data[[nms[pl[1]]]]))) |>
    filter(!is.na(Park),Park!="") |> distinct(Zone,Ward,Park)
}

# KML points. sf is required for KML import; no depth or severity is invented.
# (flood_xy is unchanged and still feeds the Flood Map page; flood_pts adds
#  KML attributes (Name/Description) for popups on the Overview map.)
flood_xy <- tibble(); flood_names <- character(); flood_desc <- character(); flood_crs_in <- NA_character_
flood_n_raw <- 0L; flood_n_empty <- 0L; flood_geom_types <- character(); flood_attr_df <- NULL; flood_attr <- NULL
if(!is.na(kml_path) && have_sf) {
  flood_raw_sf <- tryCatch(sf::st_read(kml_path,quiet=TRUE), error=function(e) { warning("KML read failed: ",conditionMessage(e)); NULL })
  flood_crs_in <- tryCatch(as.character(sf::st_crs(flood_raw_sf)$input), error=function(e) NA_character_)
  flood_sf <- tryCatch(if(is.null(flood_raw_sf)) NULL else sf::st_transform(flood_raw_sf, 4326), error=function(e) { warning("KML read failed: ",conditionMessage(e)); NULL })
  if(!is.null(flood_sf) && nrow(flood_sf)>0) {
    flood_n_raw <- nrow(flood_sf)
    flood_sf <- flood_sf[!sf::st_is_empty(flood_sf),]
    flood_n_empty <- flood_n_raw - nrow(flood_sf)
    flood_geom_types <- as.character(sf::st_geometry_type(flood_sf))
    flood_attr_df <- tryCatch(as.data.frame(sf::st_drop_geometry(flood_sf)),error=function(e) NULL)
    flood_sf <- tryCatch(if(all(as.character(sf::st_geometry_type(flood_sf))=="POINT")) flood_sf else sf::st_point_on_surface(flood_sf), error=function(e) flood_sf)
    xy <- tryCatch(sf::st_coordinates(flood_sf),error=function(e) matrix(numeric(),ncol=2))
    if(nrow(xy)>0) {
      flood_xy <- tibble(Longitude=xy[,1],Latitude=xy[,2],Record=seq_len(nrow(xy)))
      pick <- function(col) { hit <- names(flood_sf)[tolower(names(flood_sf))==tolower(col)]
      if(length(hit) && length(flood_sf[[hit[1]]])==nrow(xy)) as.character(flood_sf[[hit[1]]]) else rep(NA_character_,nrow(xy)) }
      flood_names <- pick("Name"); flood_desc <- pick("Description")
      if(!is.null(flood_attr_df) && nrow(flood_attr_df)==nrow(xy)) flood_attr <- flood_attr_df
    }
  }
}
flood_pts <- if(nrow(flood_xy)) mutate(flood_xy, Name=flood_names, Description=flood_desc) |>
  filter(is.finite(Longitude), is.finite(Latitude), between(Latitude,-90,90), between(Longitude,-180,180)) else tibble()
# Plausibility check only (points are NOT dropped): rough Chennai-region bounding box.
flood_outside <- if(nrow(flood_pts)) sum(!(between(flood_pts$Latitude,12.6,13.6) & between(flood_pts$Longitude,79.9,80.6))) else 0L

# ---------------------------------------------------------------------
# Flood Map page data model (all source rows preserved; validity flagged).
# Record = row order of the KML features after empty geometries are dropped.
# ---------------------------------------------------------------------
FLOOD_STYLE_COLS <- c("name","description","altitudemode","tessellate","extrude","visibility","draworder","icon","snippet")
flood_geo <- tibble()
if(nrow(flood_xy)) {
  flood_geo <- flood_xy |> mutate(
    Name=flood_names, Description=flood_desc,
    Geometry_type=if(length(flood_geom_types)==n()) flood_geom_types else NA_character_,
    Has_name=!is.na(Name) & nzchar(trimws(Name)), Has_desc=!is.na(Description) & nzchar(trimws(Description)),
    Has_attr=Has_name | Has_desc,
    Valid=is.finite(Longitude) & is.finite(Latitude) & between(Latitude,-90,90) & between(Longitude,-180,180),
    Coord_key=ifelse(Valid,paste0(sprintf("%.6f",Longitude),"|",sprintf("%.6f",Latitude)),NA_character_),
    Label=ifelse(Has_name,Name,paste("Recorded Point",Record)))
}
flood_valid <- if(nrow(flood_geo)) filter(flood_geo,Valid) else flood_geo
flood_ok <- nrow(flood_valid)>0
# Extra verified source attributes (only columns that actually hold values; KML styling fields are excluded)
flood_extra_df <- NULL
if(!is.null(flood_attr)) {
  ex <- flood_attr[, !(tolower(names(flood_attr)) %in% FLOOD_STYLE_COLS), drop=FALSE]
  keep <- vapply(ex,function(z) any(!is.na(z) & nzchar(trimws(as.character(z)))),logical(1))
  if(length(keep) && any(keep)) flood_extra_df <- ex[, keep, drop=FALSE]
}
# Suspected lon/lat swap check (flag only; coordinates are never silently swapped)
flood_swapped <- if(flood_ok) sum(between(flood_valid$Latitude,79.9,80.6) & between(flood_valid$Longitude,12.6,13.6)) else 0L
fl_pad <- function(r) { r <- c(floor(r[1]*1000)/1000, ceiling(r[2]*1000)/1000); if(r[1]==r[2]) r <- r+c(-.001,.001); r }
flood_bounds <- if(flood_ok) list(id=range(flood_valid$Record), lat=fl_pad(range(flood_valid$Latitude)), lon=fl_pad(range(flood_valid$Longitude))) else list(id=c(0,1), lat=c(0,1), lon=c(0,1))

# Great-circle (haversine) distance in metres on a spherical Earth (R = 6371.0088 km)
hav_dist <- function(lon1,lat1,lon2,lat2) { r <- pi/180; a <- sin((lat2-lat1)*r/2)^2 + cos(lat1*r)*cos(lat2*r)*sin((lon2-lon1)*r/2)^2; 6371008.8*2*asin(pmin(1,sqrt(a))) }
hav_matrix <- function(lon,lat) outer(seq_along(lon),seq_along(lon),function(i,j) hav_dist(lon[i],lat[i],lon[j],lat[j]))
# Proximity groups: connected components of the graph "distance <= eps" (single linkage).
# Equivalent to DBSCAN with minPts = 2 (a point counts itself); isolated points = noise (group 0).
prox_groups <- function(D, eps) {
  n <- nrow(D); grp <- integer(n); g <- 0L
  for(i in seq_len(n)) if(grp[i]==0L) {
    g <- g+1L; grp[i] <- g; queue <- i
    while(length(queue)) { j <- queue[1]; queue <- queue[-1]; nb <- which(D[j,]<=eps & grp==0L); if(length(nb)) { grp[nb] <- g; queue <- c(queue,nb) } }
  }
  sz <- table(grp); big <- as.integer(names(sz)[sz>=2]); big <- big[order(-as.integer(sz[as.character(big)]),big)]
  list(group=ifelse(grp %in% big,match(grp,big),0L), ngroups=length(big))
}
# Square grid in metres (converted to degrees at the mean latitude) for the concentration layer
flood_grid <- function(x, cs) {
  dlat <- cs/111320; dlon <- cs/(111320*cos(mean(x$Latitude)*pi/180))
  x |> mutate(ix=floor(Longitude/dlon),iy=floor(Latitude/dlat)) |> count(ix,iy,name="n") |>
    mutate(lng1=ix*dlon,lng2=(ix+1)*dlon,lat1=iy*dlat,lat2=(iy+1)*dlat)
}
flood_extra_text <- function(rec) {
  if(is.null(flood_extra_df) || rec>nrow(flood_extra_df)) return(character())
  v <- unlist(lapply(flood_extra_df[rec,,drop=FALSE],function(z) as.character(z[1]))); v[!is.na(v) & nzchar(trimws(v))]
}
flood_popup <- function(x, grp=NULL) {
  vapply(seq_len(nrow(x)),function(i) {
    r <- x[i,]; ex <- flood_extra_text(r$Record)
    paste0("<b>",htmlEscape(r$Label),"</b><br>Recorded point ID: ",r$Record,"<br>Latitude: ",sprintf("%.6f",r$Latitude),"<br>Longitude: ",sprintf("%.6f",r$Longitude),
           if(isTRUE(r$Has_name)) paste0("<br>KML name: ",htmlEscape(r$Name)),
           if(isTRUE(r$Has_desc)) paste0("<br>KML description: ",htmlEscape(substr(r$Description,1,300))),
           if(!isTRUE(r$Has_attr)) "<br><i>The source gives no name or description for this point.</i>",
           if(length(ex)) paste0("<br>",htmlEscape(names(ex)),": ",htmlEscape(ex),collapse=""),
           if(!is.null(grp)) paste0("<br>Proximity group: ",if(grp[i]>0) grp[i] else "none (isolated)"),
           "<br><i>Recorded location only: no verified depth, severity or date.</i>")
  },character(1))
}
kv_dl <- function(items) tags$dl(class="es-kv",lapply(names(items),function(n) tagList(tags$dt(n),tags$dd(items[[n]]))))


metric <- function(label,value,note="") div(class="metric-card",
  div(class="metric-label",label),
  div(class="metric-value",value),
  if(nzchar(note)) div(class="metric-note",note) else NULL
)
notice <- function(title,text,type="warning") div(class=paste("notice",type),tags$strong(title),text)
header <- function(title,subtitle) tagList(tags$h2(class="section-title",title),tags$p(class="section-subtitle",subtitle))

# ---- New UI helpers (Overview + Rainfall) ----
kpi <- function(label,value,note="") div(class="es-kpi",
  div(class="es-kpi-label",label),
  div(class="es-kpi-value",value),
  if(nzchar(note)) div(class="es-kpi-note",note) else NULL
)
chart_panel <- function(title,subtitle=NULL,...,class="") div(class=paste("panel es-panel",class),tags$h3(title),if(!is.null(subtitle)) tags$p(class="es-panel-sub",subtitle),...)
insight_item <- function(label,...) div(class="es-insight",span(class="es-insight-tag",label),span(class="es-insight-text",...))

# ---- New analytical helpers ----
fmt_n   <- function(x) format(x, big.mark=",", scientific=FALSE)
fmt_num <- function(x, d=2) ifelse(is.finite(x), formatC(x, format="f", digits=d, big.mark=","), "N/A")
cfg <- function(p) config(p, responsive=TRUE, displaylogo=FALSE, modeBarButtonsToRemove=c("lasso2d","select2d"), toImageButtonOptions=list(format="png",scale=2,filename="ecoshield_chart"))
base_layout <- function(p, ...) layout(p, paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)", font=list(family="'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif", size=12, color="#0f172a"), margin=list(l=60,r=20,t=20,b=45), ...)
station_palette <- function(n) rep(c("#0f766e","#0284c7","#d97706","#7c3aed","#059669","#e11d48","#2563eb","#ea580c"), length.out=max(n,1))[seq_len(max(n,1))]

period_floor <- function(d, unit) switch(unit, month=floor_date(d,"month"), quarter=floor_date(d,"quarter"), year=floor_date(d,"year"))
period_seq   <- function(from, to, unit) seq(from, to, by=unit)
period_label <- function(p, unit) switch(unit, month=format(p,"%b %Y"), quarter=paste0(year(p)," Q",quarter(p)), year=format(p,"%Y"))
unit_word    <- function(unit) c(month="month", quarter="quarter", year="year")[[unit]]

# Aggregate records to calendar periods. Periods without records are kept as NA (never zero).
ts_summarise <- function(df, unit="month", by_station=FALSE, fill=TRUE) {
  empty <- tibble(Period=as.Date(character()),Mean=numeric(),Median=numeric(),SD=numeric(),N=integer(),Stations=integer())
  if(is.null(df) || !nrow(df)) return(empty)
  df <- mutate(df, Period=period_floor(Date,unit))
  grp <- if(by_station) c("Station","Period") else "Period"
  s <- df |> group_by(across(all_of(grp))) |>
    summarise(Mean=mean(Rainfall),Median=median(Rainfall),SD=sd(Rainfall),N=n(),Stations=n_distinct(Station),.groups="drop")
  if(fill) {
    full <- period_seq(min(s$Period),max(s$Period),unit)
    s <- if(by_station) tidyr::complete(s,Station,Period=full,fill=list(N=0L,Stations=0L)) else tidyr::complete(s,Period=full,fill=list(N=0L,Stations=0L))
  }
  arrange(s, across(all_of(grp)))
}

# Trailing rolling mean of OBSERVED periods; needs a full window and >= half non-missing values.
roll_mean <- function(x, k) {
  n <- length(x); out <- rep(NA_real_, n)
  if(k <= 1 || n < k) return(if(k<=1) x else out)
  for(i in k:n) { w <- x[(i-k+1):i]; if(sum(!is.na(w)) >= ceiling(k/2)) out[i] <- mean(w, na.rm=TRUE) }
  out
}

build_ts_plot <- function(ts, unit="month", stat="Mean", roll=FALSE, k=12L, cps=NULL, slider=FALSE) {
  stat_lab <- if(stat=="Median") "Median of records" else "Mean of records"
  uw <- unit_word(unit)
  ts$V <- ts[[stat]]
  if(!"Station" %in% names(ts)) ts$Station <- "All selected stations"
  stns <- unique(ts$Station); pal <- station_palette(length(stns))
  p <- plot_ly()
  for(i in seq_along(stns)) {
    d <- ts[ts$Station==stns[i],]
    tip <- paste0(stns[i],"<br>",period_label(d$Period,unit),"<br>",stat_lab,": ",ifelse(is.na(d$V),"no usable records",fmt_num(d$V)),
                  "<br>Records: ",fmt_n(d$N),if(length(stns)==1) paste0("<br>Stations reporting: ",d$Stations) else "")
    p <- add_trace(p, x=d$Period, y=d$V, type="scatter", mode=if(unit=="month") "lines" else "lines+markers", name=paste0(stns[i]," (recorded)"),
                   text=tip, hoverinfo="text", connectgaps=FALSE, legendgroup=stns[i], line=list(color=pal[i],width=1.2), marker=list(color=pal[i],size=5))
    if(isTRUE(roll)) {
      r <- roll_mean(d$V, as.integer(k))
      p <- add_trace(p, x=d$Period, y=r, type="scatter", mode="lines", name=paste0(stns[i]," – ",k,"-",uw," rolling mean"),
                     text=paste0(period_label(d$Period,unit),"<br>",k,"-",uw," trailing rolling mean: ",fmt_num(r)), hoverinfo="text",
                     connectgaps=FALSE, legendgroup=stns[i], line=list(color=pal[i],width=3,dash="dash"))
    }
  }
  shp <- NULL; ann <- NULL
  if(length(cps)) {
    shp <- lapply(cps, function(d) list(type="line",x0=format(d),x1=format(d),y0=0,y1=1,yref="paper",line=list(color="#c0392b",width=1.5,dash="dot")))
    ann <- lapply(cps, function(d) list(x=format(d),y=1,yref="paper",text="change point",showarrow=FALSE,textangle=-90,xanchor="right",yanchor="top",font=list(size=10,color="#c0392b")))
  }
  p |> base_layout(xaxis=list(title="",rangeslider=if(slider) list(visible=TRUE,thickness=.07) else list(visible=FALSE)),
                   yaxis=list(title=paste0(stat_lab," (unit unverified)"),rangemode="tozero",gridcolor="#e8f0ed"),
                   legend=list(orientation="h",y=if(slider) -0.32 else -0.2), shapes=shp, annotations=ann, hovermode="closest") |> cfg()
}

# Per-station summary statistics (used in tables, plots, downloads)
station_stats <- function(df) {
  if(!nrow(df)) return(tibble())
  df |> group_by(Station) |>
    summarise(Records=n(), Mean=mean(Rainfall), Median=median(Rainfall), SD=sd(Rainfall),
              Q1=quantile(Rainfall,.25,names=FALSE), Q3=quantile(Rainfall,.75,names=FALSE), P90=quantile(Rainfall,.9,names=FALSE),
              Max=max(Rainfall), Zero_share=mean(Rainfall==0), First_date=min(Date), Last_date=max(Date),
              Years_with_data=n_distinct(year(Date)), .groups="drop") |>
    mutate(IQR=Q3-Q1, Low_coverage=Records<LOW_N)
}

month_coverage <- function(df, y1, y2) {
  total <- 12L*(y2-y1+1L)
  present <- if(nrow(df)) n_distinct(floor_date(df$Date,"month")) else 0L
  list(present=present, total=total, pct=if(total>0) 100*present/total else NA_real_)
}

# Year x month heat map. Cells below `min_n` records are drawn grey (low coverage); empty cells are blank (never zero).
heat_plot <- function(cells, zlab, colorscale, min_n=1, zmin=NULL, zmax=NULL, valfmt=function(v) fmt_num(v,2)) {
  yrs <- seq(min(cells$Year), max(cells$Year)); ny <- length(yrs)
  V <- matrix(NA_real_,12,ny); NN <- matrix(0L,12,ny)
  idx <- cbind(cells$Month, match(cells$Year,yrs)); V[idx] <- cells$Val; NN[idx] <- as.integer(cells$N)
  show <- NN>=min_n & !is.na(V)
  Z <- ifelse(show,V,NA_real_); LOW <- ifelse(!show & NN>0, 1, NA_real_)
  TXT <- matrix(paste0(rep(month.name,times=ny)," ",rep(yrs,each=12),"<br>",
                       ifelse(NN==0,"No usable records",paste0(zlab,": ",valfmt(V),"<br>Records: ",fmt_n(NN),
                                                               ifelse(NN<min_n,"<br>Low coverage (below minimum): not coloured","")))), nrow=12)
  p <- plot_ly(x=yrs,y=1:12,z=Z,type="heatmap",colorscale=colorscale,zmin=zmin,zmax=zmax,text=TXT,hoverinfo="text",xgap=1,ygap=1,
               colorbar=list(title=list(text=zlab,font=list(size=11)),len=.85,thickness=12))
  if(any(!is.na(LOW))) p <- add_trace(p,x=yrs,y=1:12,z=LOW,type="heatmap",colorscale=list(c(0,"#cfd8d5"),c(1,"#cfd8d5")),showscale=FALSE,text=TXT,hoverinfo="text",xgap=1,ygap=1)
  p |> base_layout(xaxis=list(title="Year",dtick=if(ny>20) 2 else 1,showgrid=FALSE),
                   yaxis=list(title="",tickmode="array",tickvals=1:12,ticktext=month.abb,autorange="reversed",showgrid=FALSE)) |> cfg()
}

# Anomaly baseline: per calendar month, mean/SD of monthly means over the reference period.
anomaly_calc <- function(df, ref, disp, min_cell_n) {
  m <- df |> mutate(Year=year(Date),Month=month(Date)) |> group_by(Year,Month) |>
    summarise(Mean=mean(Rainfall),N=n(),.groups="drop") |> filter(N>=min_cell_n)
  base <- m |> filter(Year>=ref[1],Year<=ref[2]) |> group_by(Month) |>
    summarise(BaseMean=mean(Mean),BaseSD=sd(Mean),NRef=n(),.groups="drop") |>
    filter(NRef>=MIN_REF_YEARS, is.finite(BaseSD), BaseSD>0)
  out <- m |> filter(Year>=disp[1],Year<=disp[2]) |> inner_join(base,by="Month") |> mutate(Dev=Mean-BaseMean, Z=Dev/BaseSD)
  list(data=out, base=base)
}

# Optional change-point detection on the DESEASONALISED monthly series (z-scores by calendar month).
cp_detect <- function(ts, min_n=3) {
  fail <- function(msg) list(ok=FALSE,msg=msg,dates=as.Date(character()),segs=NULL)
  if(!have_cp) return(fail("Package 'changepoint' is not installed. Run install.packages('changepoint') to enable this analysis."))
  s <- ts |> filter(!is.na(Mean), N>=min_n) |> mutate(M=month(Period))
  if(nrow(s)<60) return(fail(paste0("Not run: only ",nrow(s)," usable months in the current selection (at least 60 are required).")))
  cm <- s |> group_by(M) |> summarise(mu=mean(Mean),sdv=sd(Mean),k=n(),.groups="drop")
  if(nrow(cm)<12 || any(cm$k<5) || any(!is.finite(cm$sdv)) || any(cm$sdv==0))
    return(fail("Not run: every calendar month needs at least 5 usable values with non-zero variation to remove seasonality."))
  s <- left_join(s,cm,by="M") |> mutate(Z=(Mean-mu)/sdv)
  fit <- tryCatch(changepoint::cpt.mean(s$Z,method="PELT",penalty="MBIC",minseglen=12), error=function(e) NULL)
  if(is.null(fit)) return(fail("Change-point routine failed for this selection."))
  idx <- changepoint::cpts(fit)
  if(!length(idx)) return(list(ok=TRUE,msg="No mean-shift change points detected at the MBIC penalty.",dates=as.Date(character()),segs=NULL))
  starts <- c(1,idx+1); ends <- c(idx,nrow(s))
  segs <- tibble(Start=s$Period[starts],End=s$Period[ends],Months=ends-starts+1,`Mean z`=mapply(function(a,b) mean(s$Z[a:b]),starts,ends))
  list(ok=TRUE,msg=NULL,dates=s$Period[idx+1],segs=segs)
}

write_with_header <- function(df, file, header_lines) {
  writeLines(paste0("# ",header_lines), file)
  write_csv(df, file, append=TRUE, col_names=TRUE)
}

# ---- Navigation SVG Icons (Clean 16x16 crisp vectors) ----
nav_icon <- function(name) {
  svgs <- list(
    "overview" = '<svg class="es-icon" width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><rect x="3" y="3" width="7" height="9"></rect><rect x="14" y="3" width="7" height="5"></rect><rect x="14" y="12" width="7" height="9"></rect><rect x="3" y="16" width="7" height="5"></rect></svg>',
    "rainfall" = '<svg class="es-icon" width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M20 16.58A5 5 0 0 0 18 7h-1.26A8 8 0 1 0 4 15.25"></path><line x1="8" y1="19" x2="8" y2="21"></line><line x1="8" y1="13" x2="8" y2="15"></line><line x1="12" y1="21" x2="12" y2="23"></line><line x1="12" y1="15" x2="12" y2="17"></line><line x1="16" y1="19" x2="16" y2="21"></line><line x1="16" y1="13" x2="16" y2="15"></line></svg>',
    "flood"    = '<svg class="es-icon" width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><polygon points="1 6 1 22 8 18 16 22 23 18 23 2 16 6 8 2 1 6"></polygon><line x1="8" y1="2" x2="8" y2="18"></line><line x1="16" y1="6" x2="16" y2="22"></line></svg>',
    "air"      = '<svg class="es-icon" width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M9.59 4.59A2 2 0 1 1 11 8H2m10.59 11.41A2 2 0 1 0 14 16H2m15.73-8.27A2.5 2.5 0 1 1 19.5 12H2"></path></svg>',
    "parks"    = '<svg class="es-icon" width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M12 2L4 12h5l-3 6h6v4h2v-4h6l-3-6h5z"></path></svg>',
    "live"     = '<svg class="es-icon" width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M4.9 19.1C1 15.2 1 8.8 4.9 4.9"></path><path d="M7.8 16.2c-2.3-2.3-2.3-6.1 0-8.5"></path><circle cx="12" cy="12" r="2"></circle><path d="M16.2 7.8c2.3 2.3 2.3 6.1 0 8.5"></path><path d="M19.1 4.9C23 8.8 23 15.1 19.1 19"></path></svg>',
    "shield"   = '<svg class="es-icon-brand" width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M12 22s8-4 8-10V5l-8-3-8 3v7c0 6 8 10 8 10z"></path></svg>'
  )
  HTML(svgs[[name]] %||% "")
}

# ---------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------
ui <- page_navbar(
  title=div(class="brandbar",
    div(class="brand-emblem", nav_icon("shield")),
    div(class="brandmark", span("Eco",class="eco"), "Shield"),
    div(class="brand-subtitle", "Chennai Environmental Intelligence")
  ),
  window_title="EcoShield – Chennai Environmental Intelligence",
  id="nav", selected="Overview",
  theme=bs_theme(version=5, bootswatch="minty", primary="#0f766e"),
  header=tags$head(
    tags$title("EcoShield – Chennai Environmental Intelligence"),
    tags$meta(name="viewport", content="width=device-width, initial-scale=1"),
    tags$link(rel="preconnect", href="https://fonts.googleapis.com"),
    tags$link(rel="preconnect", href="https://fonts.gstatic.com", crossorigin=NA),
    tags$link(rel="stylesheet", href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=Plus+Jakarta+Sans:wght@500;600;700&display=swap"),
    tags$link(rel="stylesheet", type="text/css", href="styles.css"),
    if(file.exists("www/styles.css")) includeCSS("www/styles.css") else NULL,
    tags$script(src="app.js")
  ),
  
  # ============================ OVERVIEW ============================
  nav_panel(title=tagList(nav_icon("overview"), "Overview"), value="Overview", div(class="page-wrap es-wrap",
                                              div(class="es-header",
                                                  div(div(class="es-title","EcoShield",span("Environmental Intelligence · Chennai")),
                                                      div(class="es-sub","Exploratory dashboard combining historical rainfall records, recorded flood-related locations, air-quality records and a listed-park inventory for Chennai, Tamil Nadu. All data are historical or supplied files: this is not live monitoring and not an official alert service.")),
                                                  div(class="es-header-side",uiOutput("overview_status"))),
                                              uiOutput("overview_kpis"),
                                              div(class="es-grid es-map-grid",
                                                  chart_panel("Chennai geographic overview","Recorded flood-inundation locations from the supplied KML. Layers appear only where verified coordinates exist.",
                                                              leafletOutput("overview_map",height="440px"),uiOutput("ov_layer_status"),uiOutput("ov_map_selected")),
                                                  chart_panel("Dynamic insights","Calculated from the data and the historical period selected below.",div(class="es-scroll",uiOutput("overview_insight")))),
                                              chart_panel("Historical rainfall: time-series overview","Network-wide summary of recorded values; every point is an aggregate of individual station records. Gaps mean no usable records, not zero rainfall.",
                                                          div(class="es-controls",
                                                              sliderInput("ov_years","Historical period",min=1993,max=2023,value=c(1993,2023),sep="",width="270px"),
                                                              selectInput("ov_unit","Aggregation",choices=c("Monthly"="month","Quarterly"="quarter","Annual"="year"),width="120px"),
                                                              div(style="padding-bottom:6px",checkboxInput("ov_roll","Rolling mean",value=TRUE)),
                                                              selectInput("ov_k","Window (periods)",choices=c(3,6,12),selected=12,width="120px")),
                                                          plotlyOutput("overview_rain",height="300px"),uiOutput("ov_ts_note")),
                                              div(class="es-grid es-2",
                                                  chart_panel("Year-by-month rainfall heat map","Mean of usable records per calendar month across all stations (selected period). Grey = fewer than 5 records; blank = no records.",plotlyOutput("overview_heat",height="330px")),
                                                  chart_panel("Recorded AQI by station","Mean (dot), median (diamond) and interquartile range (bar) of supplied AQI records. Observation year is unverified.",plotlyOutput("overview_aqi",height="330px"))),
                                              chart_panel("Data-quality summary","Audit of what was loaded, excluded and retained. Counts come from the same cleaning rules used by every chart.",
                                                          div(class="es-grid es-2",DTOutput("overview_quality"),div(tags$h3(style="font-size:.95rem","Data-quality considerations"),uiOutput("quality_notes")))),
                                              div(class="footer","EcoShield | Chennai Environmental Data Exploration"))),
  
  # ============================ RAINFALL ============================
  nav_panel(title=tagList(nav_icon("rainfall"), "Rainfall"), value="Rainfall", div(class="page-wrap es-wrap",header("Historical Rainfall Analysis","Explore patterns, anomalies and coverage of recorded values across monitoring stations."),
                                              layout_columns(col_widths=c(3,3,3,3),div(class="panel es-filter",selectInput("rain_station","Monitoring station",choices="All stations")),div(class="panel es-filter",sliderInput("rain_years","Year range",min=1993,max=2023,value=c(1993,2023),sep="")),div(class="panel es-filter",selectInput("rain_view","Trend view",choices=c("Monthly trend","Quarterly average","Annual average"))),div(class="panel es-filter",numericInput("rain_min","Minimum recorded value",value=0,min=0))),
                                              layout_columns(col_widths=c(8,4),div(class="panel es-filter",selectizeInput("rain_compare","Compare stations (optional, max 5; overrides the single-station filter in the time series and seasonal charts)",choices=NULL,multiple=TRUE,options=list(maxItems=5,placeholder="Select stations to overlay"),width="100%")),
                                                             div(class="panel es-filter",numericInput("rain_min_n","Minimum records per month cell (heat map, anomalies, change points)",value=3,min=1,max=31,step=1))),
                                              uiOutput("rain_kpis"),
                                              chart_panel("Advanced time-series explorer","Recorded statistic per period (solid) with optional trailing rolling mean (dashed). Use the range slider, zoom and pan to inspect individual periods.",
                                                          div(class="es-controls",
                                                              radioButtons("rain_stat","Statistic",choices=c("Mean of records"="Mean","Median of records"="Median"),inline=TRUE),
                                                              div(style="padding-bottom:6px",checkboxInput("rain_roll","Rolling mean",value=FALSE)),
                                                              selectInput("rain_k","Window (periods)",choices=c(3,6,12),selected=12,width="120px"),
                                                              div(style="padding-bottom:6px",checkboxInput("rain_cp","Detect change points (monthly view, single series)",value=FALSE))),
                                                          plotlyOutput("rain_trend",height="450px"),uiOutput("rain_cp_note")),
                                              div(class="es-grid es-2",
                                                  chart_panel("Year-by-month rainfall heat map","Aggregation: statistic of all usable records within each year-month cell, for the stations selected above. Not a monthly total.",
                                                              div(class="es-controls",selectInput("heat_stat","Cell value",choices=c("Mean of records","Median of records","Number of records"),width="190px")),
                                                              plotlyOutput("rain_heat",height="380px")),
                                                  chart_panel("Seasonal rainfall profile","Distribution of recorded values by calendar month (all years in range). Shaded band = interquartile range.",
                                                              div(class="es-controls",
                                                                  selectInput("season_basis","Spread based on",choices=c("Individual records"="records","Yearly monthly means"="yearly"),width="190px"),
                                                                  div(style="padding-bottom:6px",checkboxInput("season_polar","Polar view",value=FALSE))),
                                                              plotlyOutput("rain_season",height="380px"),uiOutput("season_note"))),
                                              chart_panel("Rainfall anomaly analysis (statistical, not event classification)","Monthly means compared with a baseline for the same calendar month in the reference period. Anomalies are not confirmed floods, droughts or climate signals.",
                                                          div(class="es-controls",
                                                              sliderInput("anom_ref","Reference (baseline) period",min=1993,max=2023,value=c(1993,2012),sep="",width="280px"),
                                                              radioButtons("anom_method","Method",choices=c("Standardised anomaly (z-score)"="z","Deviation from baseline mean"="dev"),inline=TRUE)),
                                                          uiOutput("anom_note"),
                                                          div(class="es-grid es-map-grid",plotlyOutput("anom_heat",height="360px"),div(tags$h3(style="font-size:.95rem","Most unusual recorded months"),DTOutput("anom_table")))),
                                              div(class="es-grid es-2",
                                                  chart_panel("Top stations by average recorded rainfall","Ranked dot plot: mean (dot, coloured by record count), median (diamond) and interquartile range (bar). Top 15 stations by mean in the current selection.",plotlyOutput("rain_stations",height="440px")),
                                                  chart_panel("Station bubble explorer","Each bubble is a station. Horizontal = mean recorded value, vertical = number of records, size = record count, colour = standard deviation (variability).",plotlyOutput("rain_bubble",height="440px"))),
                                              div(class="es-grid es-2",
                                                  chart_panel("Rainfall distribution","Choose a view; percentile markers use the current selection. Rainfall records are typically skewed and contain many zeros.",
                                                              div(class="es-controls",selectInput("dist_view","View",choices=c("Histogram","Box plot by station (top 10 by records)","Empirical CDF"),width="260px"),
                                                                  div(style="padding-bottom:6px",checkboxInput("dist_log","log(1 + value) axis",value=FALSE))),
                                                              plotlyOutput("rain_distribution",height="340px"),uiOutput("dist_note")),
                                                  chart_panel("Rainfall insights for current filters","Every figure is recalculated from the filtered records.",div(class="es-scroll",uiOutput("rain_insight")))),
                                              div(class="es-grid es-2",
                                                  chart_panel("Station × year observation coverage","Record counts per station and year (blank = no usable records, which is not zero rainfall). Up to 35 stations with the most records.",plotlyOutput("rain_cov_heat",height="460px")),
                                                  chart_panel("Records per year and data-quality categories","Bars = usable records; line = stations reporting.",plotlyOutput("rain_cov_year",height="220px"),DTOutput("rain_audit_table"))),
                                              div(class="panel",tags$h3("Station statistics for current filters"),downloadButton("download_rain_stats","Download station statistics (CSV)"),downloadButton("download_rain_quality","Download data-quality summary (CSV)"),br(),br(),DTOutput("rain_stats_table")),
                                              br(),
                                              div(class="panel",tags$h3("Filtered rainfall records"),downloadButton("download_rain","Download CSV"),br(),br(),DTOutput("rain_table")),
                                              br(),
                                              div(class="panel es-method",tags$details(tags$summary("Methods, assumptions and limitations"),
                                                                                       tags$ul(
                                                                                         tags$li(tags$b("Cleaning: "),"rows need a station, a parsable date and a numeric value; exact duplicate rows are removed; station-date groups with conflicting values are excluded entirely. A recorded value of 0 is kept as a recorded zero; missing or invalid rows are excluded and never converted to zero. Months/years with no records are shown as gaps."),
                                                                                         tags$li(tags$b("Units: "),"rainfall units are unverified. Values are labelled 'recorded value'. Means and medians are statistics of individual records, not cumulative totals."),
                                                                                         tags$li(tags$b("Time series: "),"records are grouped into calendar months/quarters/years and summarised (mean or median across all records in the period and selected stations). When several stations are pooled, the station mix can differ between periods. Rolling means are trailing means of the plotted period values; a window needs a full set of periods and at least half non-missing values. Recorded values are never replaced."),
                                                                                         tags$li(tags$b("Heat map: "),"statistic over all records in each year-month cell; cells below the minimum record count are grey."),
                                                                                         tags$li(tags$b("Seasonal profile: "),"median, IQR and mean by calendar month. 'Individual records' pools all records; 'Yearly monthly means' first averages each year-month (cells meeting the minimum), so the spread reflects year-to-year variation. N shown is records or years respectively."),
                                                                                         tags$li(tags$b("Anomalies: "),"for each calendar month, baseline mean and SD are taken from yearly monthly means in the reference period (at least ", MIN_REF_YEARS," reference years required, cells with the minimum record count only). z = (month mean − baseline mean) / baseline SD. Rainfall is skewed, so z-scores are descriptive indices, not probabilities. A reference period at the start of the record may absorb long-term change."),
                                                                                         tags$li(tags$b("Change points: "),"PELT mean-shift detection (changepoint::cpt.mean, MBIC penalty, minimum segment 12 months) on the monthly mean series after removing calendar-month means and SDs. Assumes roughly independent, constant-variance values; treat as exploratory. Not evidence of climate change or any specific cause."),
                                                                                         tags$li(tags$b("Distribution: "),"percentiles and box statistics are computed on the filtered records; box whiskers use the 1.5 × IQR rule and outliers are not drawn. The log axis plots log(1 + value) so zeros stay visible."),
                                                                                         tags$li(tags$b("Coverage: "),"record counts per station-year. Stations with fewer than ", LOW_N," records are flagged low coverage (a heuristic). Differences between stations may reflect coverage rather than rainfall."),
                                                                                         tags$li(tags$b("Exports: "),"CSV downloads reflect the current filters; summary files begin with comment lines listing filters. Use the camera icon on any chart to save a PNG.")))) 
  )),
  
  # ============================ FLOOD MAP ============================
  nav_panel(title=tagList(nav_icon("flood"), "Flood Map"), value="Flood Map", div(class="page-wrap es-wrap",
                                                div(class="es-header",
                                                    div(div(class="es-title","Chennai Flood Geographic Intelligence"),
                                                        div(class="es-sub","Explore recorded geographic locations and their spatial distribution.")),
                                                    div(class="es-header-side",uiOutput("flood_status"))),
                                                uiOutput("flood_kpis"),
                                                div(class="panel es-filter",style="margin-bottom:14px",
                                                    div(class="es-controls",
                                                        sliderInput("flood_id","Recorded point ID",min=flood_bounds$id[1],max=flood_bounds$id[2],value=flood_bounds$id,step=1,sep="",width="230px"),
                                                        sliderInput("flood_lat","Latitude range (°N)",min=flood_bounds$lat[1],max=flood_bounds$lat[2],value=flood_bounds$lat,step=.001,sep="",width="230px"),
                                                        sliderInput("flood_lon","Longitude range (°E)",min=flood_bounds$lon[1],max=flood_bounds$lon[2],value=flood_bounds$lon,step=.001,sep="",width="230px"),
                                                        div(style="padding-bottom:6px",checkboxInput("flood_attr_only","Only points with a source name/description",value=FALSE)),
                                                        div(style="padding-bottom:8px",actionButton("flood_reset","Reset filters",class="btn btn-outline-primary es-btn-sm")))),
                                                div(class="es-flood-grid",
                                                    chart_panel("Recorded locations map","Points are plotted at their KML coordinates (WGS84). Click a cluster to zoom in, then click a point to inspect it.",
                                                                div(class="es-controls",
                                                                    div(style="padding-bottom:6px",checkboxInput("flood_cluster","Cluster markers",value=TRUE)),
                                                                    div(style="padding-bottom:6px",checkboxInput("flood_color_group","Colour by proximity group",value=FALSE)),
                                                                    selectInput("flood_eps","Proximity threshold",choices=c("100 m"=100,"250 m"=250,"500 m"=500,"1 km"=1000,"2 km"=2000),selected=500,width="130px"),
                                                                    selectInput("flood_cell","Concentration grid cell",choices=c("250 m"=250,"500 m"=500,"1 km"=1000,"2 km"=2000),selected=1000,width="150px"),
                                                                    div(style="padding-bottom:8px",actionButton("flood_fit","Fit to data",class="btn btn-outline-primary es-btn-sm"))),
                                                                if(have_sf) leafletOutput("flood_map",height="610px") else div(class="status-error","Install sf to read the KML: install.packages('sf').")),
                                                    div(class="panel es-panel es-side-panel",
                                                        tags$h3("Geo-spatial insights"),tags$p(class="es-panel-sub","Updates with the map selection and the filters above."),
                                                        div(class="es-side-scroll",
                                                            div(class="es-sec","Selected location"),uiOutput("flood_selected"),
                                                            div(class="es-sec","Geographic summary"),uiOutput("flood_geosummary"),
                                                            div(class="es-sec","Dynamic observations"),uiOutput("flood_insights")),
                                                        div(class="es-limit",tags$b("Data limitations: "),"These points represent recorded geographic locations from the supplied KML dataset. The available data does not provide verified flood depth, severity, event timestamps, or live flood conditions. The map must not be interpreted as a current flood-risk map."))),
                                                div(class="es-grid es-2",
                                                    chart_panel("Nearest-neighbour distances","Distance from each displayed point to its closest other displayed point (great-circle metres). Describes how recorded points are spaced; says nothing about flood severity.",plotlyOutput("flood_nn_plot",height="300px")),
                                                    chart_panel("Proximity groups","Groups of recorded points linked by gaps no larger than the threshold (single linkage = DBSCAN with minPts 2). Select a row to zoom to the group. Not flood-risk zones.",DTOutput("flood_group_table"),uiOutput("flood_group_note"))),
                                                div(class="panel",tags$h3("Recorded point records (current filters)"),p(class="small-note","Select a row to highlight and zoom to the point on the map."),downloadButton("download_flood","Download coordinates (CSV)"),br(),br(),DTOutput("flood_table")),
                                                br(),
                                                div(class="panel es-method",tags$details(tags$summary("Methods, assumptions and limitations"),tags$ul(
                                                  tags$li(tags$b("Source and CRS: "),"features are read from the KML with sf and transformed to WGS84 (EPSG:4326, longitude/latitude). Empty geometries are dropped; non-point geometries (if any) are represented by a point on the surface. A point is 'valid' when both coordinates are finite and inside lon ±180 / lat ±90. Records are never silently re-ordered or swapped; a rough Chennai bounding-box check only raises a warning."),
                                                  tags$li(tags$b("Unique coordinates: "),"identical longitude/latitude pairs after rounding to 6 decimals (about 0.1 m). Records and unique coordinates are reported separately."),
                                                  tags$li(tags$b("Distances: "),"haversine great-circle distance on a spherical Earth (R = 6,371.0088 km); adequate for city-scale spacing, not survey-grade."),
                                                  tags$li(tags$b("Proximity groups: "),"connected components of points whose pairwise distance is at most the chosen threshold; groups of one point are shown as isolated. They describe how close recorded points lie to each other, not flood risk."),
                                                  tags$li(tags$b("Concentration grid: "),"counts of recorded points in square cells (size in metres, converted to degrees at the mean displayed latitude). Counts reflect where records exist, not flood severity, depth or probability."),
                                                  tags$li(tags$b("Bounding box / centroid: "),"the smallest lon/lat rectangle around the displayed points and their arithmetic mean coordinate. Neither is a flood-affected area or a place name."),
                                                  tags$li(tags$b("Not available in the source: "),"verified flood depth, severity, event dates, ward or neighbourhood boundaries, street addresses. None are inferred. Distance analyses are skipped above 1,500 displayed points.")))) 
  )),
  
  nav_panel(title=tagList(nav_icon("air"), "Air Quality"), value="Air Quality", div(class="page-wrap aq-page",
                                                    header("Chennai Air Quality Intelligence","Explore recorded AQI values, compare monitoring stations, and understand the distribution of supplied air-quality observations."),
                                                    
                                                    div(class="aq-filters",
                                                        div(class="panel",selectInput("aq_station","Monitoring station",choices="All stations")),
                                                        div(class="panel",sliderInput("aq_range","Recorded AQI range",min=0,max=500,value=c(0,500))),
                                                        div(class="panel",selectInput("aq_pollutant_filter","Prominent pollutant",choices="All pollutants")),
                                                        div(class="panel",selectInput("aq_category_filter","Supplied AQI category",choices="All categories")),
                                                        div(class="aq-reset",actionButton("aq_reset","Reset filters",icon=icon("rotate-left"),class="btn-outline-primary"))),
                                                    uiOutput("aq_metrics"),
                                                    div(class="aq-main",
                                                        div(class="aq-col",
                                                            chart_panel("Understanding the Recorded AQI","Where the selected recorded AQI values sit on a plain numeric scale (axis spans the whole supplied dataset).",
                                                                        plotlyOutput("aq_scale",height="250px"), uiOutput("aq_scale_text")),
                                                            chart_panel("Which Stations Have Higher Recorded AQI?","Stations ranked by mean recorded AQI (dot) with the median (diamond). Hover for details.",
                                                                        plotlyOutput("aq_rank",height=aq_rank_h),
                                                                        div(class="aq-explain","This chart compares the recorded AQI values available for each station. Differences may reflect observation coverage and the composition of the supplied records. It does not show that one station is currently more polluted than another."))),
                                                        div(class="aq-col",
                                                            chart_panel("Insights from the Selected Records",NULL,uiOutput("aq_insight")),
                                                            chart_panel("Supplied AQI Category Counts","Category labels exactly as supplied in the file.",plotlyOutput("aq_categories",height="270px"),uiOutput("aq_cat_note")))),
                                                    div(class="aq-row",
                                                        chart_panel("How Does AQI Vary Across Stations?","Box plot of recorded AQI per station, ordered by median.",plotlyOutput("aq_box",height=aq_box_h),
                                                                    div(class="aq-explain","The box shows the middle portion of recorded values, the center line marks the median, the dashed line marks the mean, and points outside the whiskers may indicate unusually high or low observations. They are not treated as errors without verification. A taller box or longer whiskers mean more variable recorded values.")),
                                                        chart_panel("Distribution of Recorded AQI Values","Histogram of recorded AQI values with a smoothed density curve.",plotlyOutput("aq_hist",height=aq_box_h),
                                                                    div(class="aq-explain","This chart shows which recorded AQI ranges occur most frequently in the supplied observations. Each bar is an equal-width numeric interval; the curve is a smoothed estimate of the same distribution. The intervals are not AQI categories."))),
                                                    div(class="aq-row",
                                                        chart_panel("Pollutant Profile","Which pollutant is named as prominent in the selected records, and (if available) the measured concentrations.",
                                                                    plotlyOutput("aq_pollutants",height="240px"),
                                                                    div(class="aq-explain","The prominent pollutant is a label supplied with each record. It is a category, not a concentration, and is separate from the AQI value."),
                                                                    if(nrow(aq_poll_cols)>0) tagList(tags$h3(style="margin-top:12px","Measured pollutant concentrations"),plotlyOutput("aq_conc",height="280px"),
                                                                                                     div(class="aq-explain","Each pollutant has its own vertical axis because pollutants are measured on different scales; do not compare box heights across panels. Concentrations are not AQI values."))
                                                                    else div(class="aq-explain",tags$b("Concentration chart omitted: "),"no numeric pollutant-concentration columns with usable values were found in the supplied file.")),
                                                        chart_panel("Station AQI Bubble Explorer","Each bubble is a station: further right = higher mean, higher up = more variable values, bigger = more observations.",
                                                                    plotlyOutput("aq_bubble",height="360px"),uiOutput("aq_bubble_note"))),
                                                    if(nrow(aq_geo)>0) div(class="panel es-panel",tags$h3("Monitoring station locations"),tags$p(class="es-panel-sub","Coordinates taken from the supplied file (station median). Marker size reflects the number of records in the current selection."),leafletOutput("aq_map",height="300px")) else NULL,
                                                    div(class="panel es-panel aq-dq",tags$h3("Data quality and methods"),uiOutput("aq_quality")),
                                                    div(class="panel es-panel",tags$h3("Air-quality records"),downloadButton("download_aq","Download CSV"),br(),br(),DTOutput("aq_table")))),
  
  nav_panel(title=tagList(nav_icon("parks"), "Parks"), value="Parks", div(class="page-wrap",header("Park Distribution Across Chennai","Explore listed park names by zone and ward."),
                                       
                                        layout_columns(col_widths=c(3,3,3,3),div(class="panel",selectInput("park_zone","Zone",choices="All zones")),div(class="panel",selectInput("park_ward","Ward",choices="All wards")),div(class="panel",textInput("park_search","Search park name",placeholder="Enter part of a park name")),div(class="panel",selectInput("park_sort","Sort directory",choices=c("Name A–Z","Name Z–A","Zone","Ward")))),uiOutput("park_metrics"),
                                        div(class="panel-row",div(class="panel",tags$h3("Listed parks by zone"),plotlyOutput("park_zone_plot",height="440px")),div(class="panel",tags$h3("Wards with the fewest listed parks"),plotlyOutput("park_ward_plot",height="440px"))),
                                        div(class="panel-row",div(class="panel",tags$h3("Zone bubble comparison"),p(class="small-note","Each bubble represents a zone. Bubble size shows listed park records; the vertical position shows how many wards are represented in the filtered records. This is not a green-cover measure."),plotlyOutput("park_bubble",height="390px")),div(class="panel",tags$h3("Park directory insight"),uiOutput("park_insight"))),
                                        div(class="panel",tags$h3("Park directory"),downloadButton("download_parks","Download CSV"),br(),br(),DTOutput("park_table")))),
  
  nav_panel(title=tagList(nav_icon("live"), "Live Monitor"), value="Action Plan", div(class="page-wrap lv-page",
                                                     header("Chennai Live Environmental Monitor","Monitor reported rainfall, weather observations, and official environmental alerts using the latest available data."),
                                                     uiOutput("lv_banner"),
                                                     div(class="lv-toolbar", uiOutput("lv_refresh_info"), actionButton("lv_refresh","Refresh now",icon=icon("rotate"),class="btn-outline-primary")),
                                                     uiOutput("lv_kpis"),
                                                     div(class="lv-grid2",
                                                         chart_panel("Live data status","Status is derived from each source's own timestamps, not from this page refreshing.",uiOutput("lv_status")),
                                                         chart_panel("Insights from the retrieved data",NULL,uiOutput("lv_insights"))),
                                                     div(class="lv-grid-map",
                                                         chart_panel("Chennai rainfall station map","Markers appear only for stations with verified coordinates supplied by you (see Sources & methods).",uiOutput("lv_map_ui")),
                                                         chart_panel("Selected station",NULL,uiOutput("lv_selected"))),
                                                     div(class="lv-grid2",
                                                         chart_panel("Station-wise reported rainfall","Reported 24-hour rainfall per station (millimetres; centimetres = mm ÷ 10). Click a bar to select a station.",
                                                                     div(class="lv-ctl",selectInput("lv_net","Network",choices="All networks"),selectInput("lv_top","Show",choices=c("Top 20"="20","Top 40"="40","All stations"="0"),selected="20")),
                                                                     uiOutput("lv_rank_ui")),
                                                         chart_panel("Rainfall time series","Only daily 24-hour reports this dashboard has itself retrieved; nothing is back-filled or invented.",
                                                                     div(class="lv-ctl",dateRangeInput("lv_dates","Date range",start=Sys.Date()-30,end=Sys.Date()),selectizeInput("lv_ts_station","Stations (blank = mean of reporting stations)",choices=NULL,multiple=TRUE)),
                                                                     plotlyOutput("lv_ts",height="380px"),uiOutput("lv_ts_note"))),
                                                     div(class="lv-grid2",
                                                         chart_panel("Latest weather observations","Observed values only. Forecast values are never shown here.",uiOutput("lv_wx")),
                                                         chart_panel("Official weather alerts","IMD district warning for Chennai (forecast warnings, Day 1 to Day 5).",uiOutput("lv_alerts"))),
                                                     div(class="panel es-panel",tags$h3("Station readings"),tags$p(class="es-panel-sub","Every station in the latest report, including any with missing or invalid values (shown as Unavailable, never as zero)."),
                                                         downloadButton("lv_dl_readings","Download readings (CSV)"),downloadButton("lv_dl_coords","Download coordinate template (CSV)"),br(),br(),DTOutput("lv_table")),
                                                     div(class="panel es-panel",tags$h3("Sources, methods and limitations"),uiOutput("lv_methods")))),
)

# =====================================================================
# LIVE ENVIRONMENTAL MONITOR (replaces the former Action Plan page)
# Layers kept separate:  retrieval -> parsing -> validation -> shared state -> rendering
#
# Sources (see "Sources, methods and limitations" on the page):
#  A. TN-SMART Chennai daily rainfall (HTML table; NO documented API; coordinates are NOT supplied)
#  B. IMD public API (current_wx, districtwarning): documented, but IMD requires the calling
#     server's public IP to be whitelisted, so it is OFF unless ECOSHIELD_IMD_ENABLED=true
#  C. IMD Chennai RMC portal: link only (no documented programmatic access)
# Optional packages (page degrades gracefully if missing): httr2, xml2
# =====================================================================
LIVE_TZ <- "Asia/Kolkata"
lv_env_num <- function(name, default, lo=-Inf) { v <- suppressWarnings(as.numeric(Sys.getenv(name,""))); if(is.na(v)) default else max(lo,v) }
lv_env_bool <- function(name, default) { v <- tolower(Sys.getenv(name,"")); if(!nzchar(v)) default else v %in% c("true","1","yes","y") }
live_cfg <- list(
  refresh_min  = lv_env_num("ECOSHIELD_REFRESH_MIN", 15, lo=10),   # never polls faster than every 10 min
  min_gap_sec  = 120,                                               # manual "Refresh now" cannot hit a source more than once per 2 min
  tn_enabled   = lv_env_bool("ECOSHIELD_TNSMART_ENABLED", TRUE),
  tn_url       = Sys.getenv("ECOSHIELD_TNSMART_URL","https://beta-tnsmart.rimes.int/index.php/Rainfall/gcc_day"),
  tn_file      = Sys.getenv("ECOSHIELD_TNSMART_FILE",""),          # optional saved HTML for offline testing
  imd_enabled  = lv_env_bool("ECOSHIELD_IMD_ENABLED", FALSE),
  imd_base     = "https://api.imd.gov.in/api/v1",
  imd_wx_pattern       = Sys.getenv("ECOSHIELD_IMD_WX_PATTERN","chennai|nungambakkam|meenambakkam"),
  imd_district_pattern = Sys.getenv("ECOSHIELD_IMD_DISTRICT_PATTERN","^chennai$"),
  coords_file  = { v <- Sys.getenv("ECOSHIELD_STATION_COORDS",""); if(nzchar(v)) v else if(file.exists("station_coordinates.csv")) "station_coordinates.csv" else if(file.exists(file.path("data","station_coordinates.csv"))) file.path("data","station_coordinates.csv") else "" },  # CSV of VERIFIED coordinates (auto-detected)
  cache_dir    = Sys.getenv("ECOSHIELD_CACHE_DIR","cache"),
  fresh_daily_h = 30,   # a daily 08:30 report counts as "recently updated" for 30 h (heuristic, documented)
  fresh_obs_h   = 3     # an observation counts as "live / recently updated" for 3 h (heuristic, documented)
)
LV_LINKS <- list(
  tn   = "https://beta-tnsmart.rimes.int/index.php/Rainfall/gcc_day",
  imd_ref = "https://api.imd.gov.in/public/api_reference.html",
  imd_apis = "https://mausam.imd.gov.in/responsive/apis.php",
  imd_whitelist = "https://city.imd.gov.in/citywx/api_request.php",
  imd_chennai = "https://mausam.imd.gov.in/chennai/")

# ---- Rainfall classes: ranges shown on the TN-SMART legend (same classes as the IMD 24-h scale) ----
LV_CLASSES <- c("No rain","Very light rain","Light rain","Moderate rain","Heavy rain","Very heavy rain","Extremely heavy rain")
LV_CLASS_RANGES <- c("0 mm","0.1 – 2.4 mm","2.5 – 15.5 mm","15.6 – 64.4 mm","64.5 – 115.5 mm","115.6 – 204.4 mm","> 204.4 mm")
LV_CLASS_COL <- c("#e3ebe8","#bfe6f0","#7cc6e4","#3d96d4","#f0b13a","#e0612f","#a3162a")
lv_rain_class <- function(mm) factor(as.character(cut(mm, breaks=c(-Inf,0,2.4,15.5,64.4,115.5,204.4,Inf), labels=LV_CLASSES, right=TRUE)), levels=LV_CLASSES)

# ---- IMD code tables (copied from the IMD API reference) ----
LV_WARN_CODES <- c("1"="No Warning","2"="Heavy Rain","3"="Heavy Snow","4"="Thunderstorm & Lightning, Squall etc","5"="Hailstorm","6"="Dust Storm","7"="Dust Raising Winds","8"="Strong Surface Winds","9"="Heat Wave","10"="Hot Day","11"="Warm Night","12"="Cold Wave","13"="Cold Day","14"="Ground Frost","15"="Fog","16"="Very Heavy Rain","17"="Extremely Heavy Rain")
LV_WARN_LEVEL <- c("1"="Green · no advisory","2"="Yellow · be aware","3"="Orange · be prepared","4"="Red · take action")
LV_WARN_COL   <- c("1"="#7cfc00","2"="#ffff00","3"="#ffa500","4"="#ff0000")
LV_WIND_DIR <- c("0"="Calm","20"="North-northeasterly","50"="Northeasterly","70"="East-northeasterly","90"="Easterly","110"="East-southeasterly","140"="Southeasterly","160"="South-southeasterly","180"="Southerly","200"="South-southwesterly","230"="Southwesterly","250"="West-southwesterly","270"="Westerly","290"="West-northwesterly","320"="Northwesterly","340"="North-northwesterly","360"="Northerly")
LV_WX_CODES <- c("01"="Clouds generally dissolving or becoming less developed","02"="State of sky on the whole unchanged","03"="Clouds generally forming or developing","04"="Visibility reduced by smoke","05"="Haze","06"="Widespread dust in suspension in the air","07"="Dust or sand raised by wind","08"="Well-developed dust/sand whirl(s)","09"="Duststorm or sandstorm within sight","10"="Mist","11"="Patches of shallow fog","12"="More or less continuous shallow fog","13"="Lightning visible, no thunder heard","14"="Precipitation within sight, not reaching ground","15"="Precipitation within sight, distant (> 5 km)","16"="Precipitation within sight, near but not at station","17"="Thunderstorm, no precipitation at observation time","18"="Squalls","19"="Funnel cloud(s)","20"="Drizzle (not freezing) or snow grains, not showers","21"="Rain (not freezing), not showers","22"="Snow, not showers","23"="Rain and snow / ice pellets, not showers","24"="Freezing drizzle / freezing rain","25"="Showers of rain","26"="Showers of snow, or rain and snow","27"="Showers of hail, or rain and hail","28"="Fog or ice fog","29"="Thunderstorm (with or without precipitation)","30"="Slight/moderate duststorm - decreased","31"="Slight/moderate duststorm - no change","32"="Slight/moderate duststorm - begun/increased","33"="Severe duststorm - decreased","34"="Severe duststorm - no change","35"="Severe duststorm - begun/increased","36"="Slight/moderate blowing snow, low","37"="Heavy drifting snow, low","38"="Slight/moderate blowing snow, high","39"="Heavy drifting snow, high","40"="Fog at a distance","41"="Fog in patches","42"="Fog, sky visible, thinning","43"="Fog, sky invisible, thinning","44"="Fog, sky visible, no change","45"="Fog, sky invisible, no change","46"="Fog, sky visible, thickening","47"="Fog, sky invisible, thickening","48"="Fog depositing rime, sky visible","49"="Fog depositing rime, sky invisible","50"="Drizzle, intermittent, slight","51"="Drizzle, continuous, slight","52"="Drizzle, intermittent, moderate","53"="Drizzle, continuous, moderate","54"="Drizzle, intermittent, heavy","55"="Drizzle, continuous, heavy","56"="Freezing drizzle, slight","57"="Freezing drizzle, moderate or heavy","58"="Drizzle and rain, slight","59"="Drizzle and rain, moderate or heavy","60"="Rain, intermittent, slight","61"="Rain, continuous, slight","62"="Rain, intermittent, moderate","63"="Rain, continuous, moderate","64"="Rain, intermittent, heavy","65"="Rain, continuous, heavy","66"="Freezing rain, slight","67"="Freezing rain, moderate or heavy","68"="Rain/drizzle and snow, slight","69"="Rain/drizzle and snow, moderate or heavy","70"="Intermittent snowflakes, slight","71"="Continuous snowflakes, slight","72"="Intermittent snowflakes, moderate","73"="Continuous snowflakes, moderate","74"="Intermittent snowflakes, heavy","75"="Continuous snowflakes, heavy","76"="Ice prisms","77"="Snow grains","78"="Isolated star-like snow crystals","79"="Ice pellets","80"="Rain shower(s), slight","81"="Rain shower(s), moderate or heavy","82"="Rain shower(s), violent","83"="Rain and snow showers, slight","84"="Rain and snow showers, moderate or heavy","85"="Snow shower(s), slight","86"="Snow shower(s), moderate or heavy","87"="Snow/ice pellet shower(s), slight","88"="Snow/ice pellet shower(s), moderate or heavy","89"="Hail shower(s), slight","90"="Hail shower(s), moderate or heavy","91"="Slight rain; thunderstorm during preceding hour","92"="Moderate/heavy rain; thunderstorm during preceding hour","93"="Slight snow/hail; thunderstorm during preceding hour","94"="Moderate/heavy snow/hail; thunderstorm during preceding hour","95"="Thunderstorm, slight/moderate, with rain, no hail","96"="Thunderstorm, slight/moderate, with hail","97"="Thunderstorm, heavy, with rain, no hail","98"="Thunderstorm with duststorm/sandstorm","99"="Thunderstorm, heavy, with hail")

# ---- Formatting helpers ----
lv_norm <- function(n) gsub("[^a-z0-9]","",tolower(n))
lv_fmt_t <- function(t) if(length(t)==0 || is.na(t[1])) "Unavailable" else paste0(format(with_tz(t[1],LIVE_TZ),"%d %b %Y, %H:%M")," IST")
lv_mm   <- function(v) ifelse(is.finite(v), paste0(formatC(v,format="f",digits=1,big.mark=",")," mm"), "Unavailable")
lv_cm   <- function(v) ifelse(is.finite(v), paste0(formatC(v/10,format="f",digits=2,big.mark=",")," cm"), "Unavailable")   # cm = mm / 10
lv_mmcm <- function(v) ifelse(is.finite(v), paste0(lv_mm(v)," · ",lv_cm(v)), "Unavailable")
lv_network <- function(d) { d <- as.character(d)
if(grepl("\u0bae\u0bbe\u0ba8\u0b95\u0bb0\u0bbe\u0b9f\u0bcd\u0b9a\u0bbf",d)) "Greater Chennai Corporation gauges"
else if(grepl("\u0bb5\u0bb0\u0bc1\u0bb5\u0bbe\u0baf\u0bcd",d)) "Revenue Department stations" else d }
lv_period_label <- function(meta) {
  if(isTRUE(meta$period_ok)) paste0("08:30 IST ",format(meta$rep_date-1,"%d %b")," to 08:30 IST ",format(meta$rep_date,"%d %b %Y")," (24-hour accumulation)")
  else if(!is.null(meta$period_text) && nzchar(meta$period_text)) paste0("As stated by source: ",meta$period_text) else "Reporting period unavailable" }
lv_parse_utc <- function(d,t) {
  hh <- gsub("[^0-9]","",as.character(t))
  hh <- ifelse(nchar(hh)>=3, sprintf("%04d",suppressWarnings(as.integer(substr(hh,1,4)))), ifelse(nchar(hh)>0, sprintf("%02d00",suppressWarnings(as.integer(hh))), NA_character_))
  as.POSIXct(paste0(as.character(d)," ",substr(hh,1,2),":",substr(hh,3,4)),format="%Y-%m-%d %H:%M",tz="UTC") }
lv_wind_dir <- function(v) { if(!is.finite(v)) return("Unavailable"); k <- as.character(round(v)); if(k %in% names(LV_WIND_DIR)) paste0(LV_WIND_DIR[[k]]," (code ",k,")") else paste0("code ",k," (not in IMD table)") }
lv_wx_desc <- function(v) { if(!is.finite(v)) return("Unavailable"); k <- sprintf("%02d",as.integer(round(v))); if(k %in% names(LV_WX_CODES)) LV_WX_CODES[[k]] else paste0("code ",k," (not in IMD table)") }
lv_warn_desc <- function(codes) { if(is.na(codes)||!nzchar(trimws(codes))) return("Unavailable"); z <- trimws(strsplit(as.character(codes),",")[[1]]); paste(ifelse(z %in% names(LV_WARN_CODES), LV_WARN_CODES[z], paste0("Code ",z," (not in IMD reference)")),collapse="; ") }

# ---- RETRIEVAL (HTTP only; no parsing here) ----
live_http_get <- function(url, accept="text/html,application/json") {
  if(!requireNamespace("httr2",quietly=TRUE)) stop("Package 'httr2' is not installed: install.packages('httr2')")
  req <- httr2::request(url) |>
    httr2::req_user_agent("EcoShield-Dashboard/1.0 (non-commercial; low-frequency polling)") |>
    httr2::req_headers(Accept=accept) |> httr2::req_timeout(15) |>
    httr2::req_retry(max_tries=2, is_transient=function(resp) httr2::resp_status(resp) %in% c(500,502,503,504)) |>
    httr2::req_error(is_error=function(resp) FALSE)
  resp <- httr2::req_perform(req)
  list(status=httr2::resp_status(resp), body=httr2::resp_body_string(resp,encoding="UTF-8"), headers=httr2::resp_headers(resp))
}
imd_get_json <- function(path) {
  r <- live_http_get(paste0(live_cfg$imd_base,"/",path),"application/json")
  if(r$status==429) stop("IMD API rate limit reached (HTTP 429)")
  if(r$status %in% c(401,403)) stop(sprintf("IMD API refused the request (HTTP %s). IMD only serves whitelisted public IPs.",r$status))
  if(r$status>=400) stop(sprintf("IMD API returned HTTP %s",r$status))
  jsonlite::fromJSON(r$body, simplifyDataFrame=TRUE)
}
imd_df <- function(x) {
  if(is.list(x) && !is.data.frame(x) && !is.null(x$data)) x <- x$data
  if(is.data.frame(x)) return(as_tibble(x))
  if(is.list(x) && length(x)) return(as_tibble(dplyr::bind_rows(lapply(x,function(e) as.data.frame(lapply(e,function(z) if(length(z)) as.character(z[1]) else NA_character_),stringsAsFactors=FALSE)))))
  stop("Unexpected IMD response structure (no records)")
}

# ---- PARSING: TN-SMART HTML -> raw rows + report metadata ----
# The station table is identified by its STRUCTURE AND CONTENT (rows that start with a whole-number sequence and end
# with a rainfall value), NOT by a fixed table index and NOT by an exact cell count. Cells are read from both <td> and <th>.
LV_MAX_PLAUSIBLE_MM <- 1000      # sanity bound for a single 24-h station reading; larger values are rejected as malformed
lv_fmt_d <- function(d) if(length(d)==0 || is.null(d) || is.na(d[1])) "Unavailable" else format(d[1],"%d %b %Y")
lv_log <- function(level, ...) message(sprintf("[EcoShield %s] %s TN-SMART: %s", format(Sys.time(),"%Y-%m-%d %H:%M:%S"), level, paste0(...)))
lv_clean_txt <- function(x) {                       # HTML entities are already decoded by xml2; this removes nbsp / zero-width characters and squishes whitespace
  x <- as.character(x); x[is.na(x)] <- ""
  x <- gsub("[\u00a0\u2007\u202f\u200b\u200c\u200d\ufeff]"," ",x,perl=TRUE)
  stringr::str_squish(x)
}
lv_num_strict <- function(x) {                      # "0", "0.0", "151", "1,234.5", "12 mm" -> numeric; "", "-", "NA", "n/a", "1e3", "Inf" -> NA (never 0)
  s <- gsub(",","",lv_clean_txt(x),fixed=TRUE); s <- sub("(?i)\\s*mm$","",s,perl=TRUE)
  ok <- grepl("^[0-9]+(\\.[0-9]+)?$",s); out <- rep(NA_real_,length(s)); out[ok] <- as.numeric(s[ok]); out
}
TN_PERIOD_RE <- "(?i)8\\s*[.:]\\s*30\\s*a\\s*\\.?\\s*m\\s*\\.?\\s*(?:of|on)?\\s*(?:the\\s*)?previous\\s*day\\s*to\\s*8\\s*[.:]\\s*30\\s*a\\s*\\.?\\s*m\\s*\\.?"
tn_row_cells <- function(tr) lv_clean_txt(xml2::xml_text(xml2::xml_find_all(tr,"./td|./th")))
tn_is_station_row <- function(z) length(z) >= 4 && grepl("^[0-9]{1,5}$",z[1])
parse_tnsmart <- function(html) {
  if(!requireNamespace("xml2",quietly=TRUE)) stop("Package 'xml2' is not installed: install.packages('xml2')")
  doc <- tryCatch(xml2::read_html(html),error=function(e) stop("The TN-SMART response could not be read as HTML: ",conditionMessage(e),call.=FALSE))
  tabs <- xml2::xml_find_all(doc,"//table")
  if(!length(tabs)) stop("No HTML <table> was found in the TN-SMART response; the page may now build its table with JavaScript, or an error/maintenance page was returned.")
  # 1) score every table by how many rows look like station records (leading sequence number, trailing numeric rainfall)
  cand <- lapply(seq_along(tabs),function(i) {
    cells <- lapply(xml2::xml_find_all(tabs[[i]],".//tr[not(.//tr)]"),tn_row_cells)
    st <- Filter(tn_is_station_row,cells)
    raw <- vapply(st,function(z) z[length(z)],character(1))
    list(index=i,cells=cells,st=st,n_numeric=as.numeric(sum(is.finite(lv_num_strict(raw)))))
  })
  score <- vapply(cand,function(z) z$n_numeric,numeric(1))
  if(max(score) < 3) stop(sprintf("No station-wise rainfall table was identified: %d table(s) inspected; the best candidate had %d row(s) with a sequence number and a numeric rainfall value (at least 3 needed). The page layout may have changed or the table is empty.",length(tabs),as.integer(max(score))))
  best <- cand[[which.max(score)]]; st <- best$st
  # 2) map cells by POSITION FROM BOTH ENDS: first = sequence, last = rainfall, second-last = station, anything in between = reporting department
  rows <- tibble(Seq=suppressWarnings(as.integer(vapply(st,function(z) z[1],character(1)))),
                 District_raw=vapply(st,function(z) paste(z[2:(length(z)-2)],collapse=" "),character(1)),
                 Station=vapply(st,function(z) z[length(z)-1],character(1)),
                 Raw=vapply(st,function(z) z[length(z)],character(1)))
  # 3) source reporting date and observation period: only what the page states (never invented)
  nodes <- xml2::xml_find_all(doc,"//text()[not(ancestor::script) and not(ancestor::style)]")
  txt <- stringr::str_squish(paste(lv_clean_txt(xml2::xml_text(nodes)),collapse=" "))
  m <- stringr::str_match(txt,"(?i)rainfall\\s*data\\s*(?:on|for|of)?\\s*(\\d{1,2})\\s*[-/. ]\\s*([A-Za-z]{3,9}|\\d{1,2})\\s*[-/. ]\\s*(\\d{4})\\s*(?:\\(([^)]*)\\))?")
  rep_date <- as.Date(NA); ptxt <- NA_character_
  if(!is.na(m[1,1])) {
    mon <- if(grepl("^\\d+$",m[1,3])) as.integer(m[1,3]) else match(tolower(substr(m[1,3],1,3)),tolower(month.abb))
    rep_date <- if(is.na(mon)) as.Date(NA) else as.Date(sprintf("%04d-%02d-%02d",as.integer(m[1,4]),mon,as.integer(m[1,2])),format="%Y-%m-%d")
    ptxt <- stringr::str_squish(m[1,5])
  }
  has_ptxt <- !is.na(ptxt) && nzchar(ptxt)
  pm <- stringr::str_extract(if(has_ptxt) ptxt else txt,TN_PERIOD_RE)
  period_ok <- !is.na(pm) && !is.na(rep_date)
  period_text <- if(has_ptxt) ptxt else if(!is.na(pm)) stringr::str_squish(pm) else ""
  obs_end <- if(period_ok) as.POSIXct(paste(rep_date,"08:30:00"),tz=LIVE_TZ) else as.POSIXct(NA_character_,tz=LIVE_TZ)
  avg_row <- Filter(function(z) { nz <- z[nzchar(z)]; length(nz)>=2 && grepl("^(average|avg\\.?)$",nz[1],ignore.case=TRUE) },best$cells)
  src_avg <- if(length(avg_row)) lv_num_strict(avg_row[[1]][length(avg_row[[1]])])[1] else NA_real_
  list(rep_date=rep_date, period_text=period_text, period_ok=period_ok, obs_end=obs_end, rows=rows, src_avg=src_avg,
       diag=list(n_tables=length(tabs), table_index=best$index, n_candidates=length(st), n_numeric=best$n_numeric,
                 seq_increasing=all(diff(rows$Seq) > 0, na.rm=TRUE), header_found=!is.na(m[1,1]), date_ok=!is.na(rep_date)))
}

# ---- VALIDATION: station rows -> typed, flagged data (missing is NEVER turned into zero; valid 0 mm is kept) ----
validate_rain <- function(p, last_modified=NA_character_) {
  if(is.null(p$rows) || !nrow(p$rows)) stop("The parser returned no station rows.")
  x <- p$rows |> mutate(District_raw=lv_clean_txt(District_raw), Station=lv_clean_txt(Station), Raw=lv_clean_txt(Raw),
                        Network=vapply(District_raw,lv_network,character(1),USE.NAMES=FALSE),
                        Rainfall_mm=lv_num_strict(Raw),
                        Zone=suppressWarnings(as.integer(stringr::str_match(Station,"(?i)^Z(?:one)?\\s*[-.]?\\s*(\\d+)")[,2])),
                        Ward=suppressWarnings(as.integer(stringr::str_match(Station,"(?i)(?:\\(\\s*W|(?<![A-Za-z])Ward)\\s*[-.]?\\s*(\\d+)")[,2])))
  # Repeated station entries are never averaged: an identical repeat is counted once; conflicting repeats are all rejected.
  key <- paste(x$District_raw,tolower(x$Station),sep="||")
  dup_n <- as.numeric(stats::ave(seq_along(key),key,FUN=length))
  same <- as.numeric(stats::ave(x$Rainfall_mm,key,FUN=function(v) as.numeric(all(is.finite(v)) && length(unique(v))==1)))
  dup_conflict <- dup_n>1 & same!=1
  dup_repeat <- dup_n>1 & same==1 & duplicated(key)
  x$Issue <- dplyr::case_when(is.na(x$Seq) ~ "missing sequence number",
                              !nzchar(x$Station) | grepl("^[0-9.,[:space:]]+$",x$Station,perl=TRUE) ~ "missing station name",
                              !nzchar(x$Raw) ~ "missing rainfall value",
                              !is.finite(x$Rainfall_mm) ~ "non-numeric rainfall value",
                              x$Rainfall_mm < 0 ~ "negative rainfall",
                              x$Rainfall_mm > LV_MAX_PLAUSIBLE_MM ~ paste0("implausible rainfall (> ",LV_MAX_PLAUSIBLE_MM," mm)"),
                              dup_conflict ~ "conflicting repeated station entries (not averaged)",
                              dup_repeat ~ "repeated station entry (counted once)",
                              TRUE ~ "")
  x <- x |> mutate(Valid=Issue=="", Rainfall_mm=ifelse(Valid,Rainfall_mm,NA_real_), Rainfall_cm=Rainfall_mm/10,
                   Class=lv_rain_class(Rainfall_mm), Label=make.unique(Station), Period=lv_period_label(p),
                   Obs_end=p$obs_end, Source="TN-SMART (Greater Chennai daily rainfall report)")
  calc <- if(any(x$Valid)) mean(x$Rainfall_mm[x$Valid]) else NA_real_   # parser integrity check against the source's own average row only
  list(data=x,
       meta=list(obs_time=p$obs_end, rep_date=p$rep_date, period_ok=p$period_ok, period_text=p$period_text, src_avg=p$src_avg, last_modified=last_modified, diag=p$diag),
       audit=list(rows=nrow(x), valid=sum(x$Valid), invalid=sum(!x$Valid), dups=sum(dup_conflict | dup_repeat), zeros=sum(x$Valid & x$Rainfall_mm==0), calc_avg=calc,
                  avg_match=is.finite(p$src_avg) && is.finite(calc) && abs(p$src_avg-calc)<=0.011))
}
fetch_tnsmart <- function() {
  if(nzchar(live_cfg$tn_file)) { html <- paste(readLines(live_cfg$tn_file,warn=FALSE,encoding="UTF-8"),collapse="\n"); lm <- NA_character_ }
  else {
    r <- tryCatch(live_http_get(live_cfg$tn_url),error=function(e) {
      msg <- conditionMessage(e)
      kind <- if(grepl("time(d)? ?out|timeout",msg,ignore.case=TRUE)) "request timed out" else "request failed (network, DNS or TLS)"
      stop(sprintf("TN-SMART %s: %s",kind,substr(msg,1,200)),call.=FALSE) })
    if(r$status==429) stop("TN-SMART rate limit reached (HTTP 429)")
    if(r$status>=400) stop(sprintf("TN-SMART returned HTTP %s",r$status))
    html <- r$body; lm <- r$headers[[ "last-modified" ]] %||% NA_character_ }
  if(is.null(html) || !nzchar(trimws(html)) || nchar(html) < 500) stop("TN-SMART returned an empty or truncated response")
  p <- tryCatch(parse_tnsmart(html),error=function(e) { lv_log("ERROR","parsing failed: ",conditionMessage(e)); stop(sprintf("TN-SMART page could not be parsed: %s",conditionMessage(e)),call.=FALSE) })
  res <- validate_rain(p,lm)
  lv_log("INFO",res$audit$valid," valid of ",res$audit$rows," station rows (table #",p$diag$table_index," of ",p$diag$n_tables,"); source date ",lv_fmt_d(p$rep_date),
         "; period ",if(isTRUE(p$period_ok)) "recognised" else "NOT recognised")
  if(res$audit$invalid>0) lv_log("WARN",res$audit$invalid," row(s) rejected: ",paste(unique(res$data$Issue[!res$data$Valid]),collapse="; "))
  if(!isTRUE(p$period_ok)) lv_log("WARN","observation period text not recognised; rainfall shown without an assumed period")
  res
}

# ---- DIAGNOSTIC: source("app.R") in RStudio (this does NOT launch the app), then run  tnsmart_diagnose()  or  tnsmart_diagnose(file="saved_page.html") ----
tnsmart_diagnose <- function(file=NULL, n_show=10) {
  cat("=== EcoShield TN-SMART parser diagnostic ===\n")
  if(!is.null(file) && nzchar(file)) { cat("Input: local file ",file,"\n",sep=""); html <- paste(readLines(file,warn=FALSE,encoding="UTF-8"),collapse="\n") }
  else { cat("Input: ",live_cfg$tn_url,"\n",sep=""); r <- live_http_get(live_cfg$tn_url); cat("HTTP status: ",r$status,"\n",sep="")
  if(r$status>=400) stop("DIAGNOSTIC FAILED: HTTP status ",r$status,call.=FALSE); html <- r$body }
  cat("HTML size (characters): ",nchar(html),"\n",sep="")
  p <- tryCatch(parse_tnsmart(html),error=function(e) stop("DIAGNOSTIC FAILED (expected structure absent): ",conditionMessage(e),call.=FALSE))
  v <- validate_rain(p); d <- v$data; g <- p$diag
  cat("Tables in page: ",g$n_tables,"; station table identified: #",g$table_index," (by structure, not by fixed index)\n",sep="")
  cat("Extracted station records: ",nrow(d)," (valid ",v$audit$valid,", rejected ",v$audit$invalid,")\n",sep="")
  cat("\nFirst ",min(n_show,nrow(d))," records:\n",sep="")
  print(as.data.frame(head(d[,c("Seq","Network","Station","Raw","Rainfall_mm","Issue")],n_show)),row.names=FALSE)
  chk <- c(`station table has >= 3 numeric rows`=g$n_numeric>=3,
           `sequence numbers increase`=isTRUE(g$seq_increasing),
           `all valid rainfall values numeric, finite and >= 0`=all(is.finite(d$Rainfall_mm[d$Valid])) && all(d$Rainfall_mm[d$Valid]>=0),
           `no missing value silently turned into 0`=all(is.na(d$Rainfall_mm[!d$Valid])),
           `source reporting date found`=isTRUE(g$date_ok),
           `observation period recognised`=isTRUE(p$period_ok))
  zr <- d[d$Valid & d$Rainfall_mm==0,]
  cat("\nZero-rainfall readings preserved as valid 0 mm: ",nrow(zr),"\n",sep="")
  if(nrow(d[d$Valid,])) { top <- d[d$Valid,][which.max(d$Rainfall_mm[d$Valid]),]; cat("Highest valid reading: ",top$Station," = ",top$Rainfall_mm," mm (",top$Rainfall_mm/10," cm)\n",sep="") }
  cat("Source reporting date: ",lv_fmt_d(p$rep_date),"\n",sep="")
  cat("Source period text: ",if(nzchar(p$period_text)) p$period_text else "(none found)","\n",sep="")
  cat("Observation period end (derived only if period recognised): ",lv_fmt_t(p$obs_end),"\n",sep="")
  cat("Page retrieved at: ",format(Sys.time(),"%d %b %Y, %H:%M:%S")," (local clock; separate from the observation period)\n",sep="")
  if(is.finite(p$src_avg)) cat("Source 'Average' row: ",p$src_avg," mm; recomputed from valid rows: ",round(v$audit$calc_avg,2)," mm (parser integrity check only)\n",sep="")
  cat("\nChecks:\n"); for(n in names(chk)) cat("  [",if(isTRUE(chk[[n]])) "PASS" else "FAIL","] ",n,"\n",sep="")
  if(v$audit$valid==0) stop("DIAGNOSTIC FAILED: no valid station readings were extracted.",call.=FALSE)
  if(!all(chk)) cat("\nRESULT: PARSED WITH WARNINGS (see FAIL lines above)\n") else cat("\nRESULT: PASS\n")
  invisible(v)
}

# ---- IMD retrieval + validation (only used when ECOSHIELD_IMD_ENABLED=true) ----
fetch_imd_wx <- function() {
  id <- Sys.getenv("ECOSHIELD_IMD_WX_STATION_ID","")
  x <- imd_df(imd_get_json(if(nzchar(id)) paste0("current_wx?id=",utils::URLencode(id,reserved=TRUE)) else "current_wx"))
  names(x) <- lv_norm(names(x))
  need <- c("station","temperature","humidity","dateofobservation","timeofobservation")
  if(!all(need %in% names(x))) stop(paste("Unexpected IMD current-weather fields; missing:",paste(setdiff(need,names(x)),collapse=", ")))
  if(!nzchar(id)) x <- x[grepl(live_cfg$imd_wx_pattern,x$station,ignore.case=TRUE),]
  if(!nrow(x)) stop("No Chennai station found in the IMD current-weather response (adjust ECOSHIELD_IMD_WX_PATTERN or set ECOSHIELD_IMD_WX_STATION_ID).")
  get <- function(n) if(n %in% names(x)) as.character(x[[n]]) else rep(NA_character_,nrow(x))
  num <- function(n) suppressWarnings(as.numeric(get(n)))
  d <- tibble(Station=get("station"), Date=get("dateofobservation"), Time_raw=get("timeofobservation"), Temp=num("temperature"), RH=num("humidity"),
              WindSpeed=num("windspeed"), WindDir=num("winddirection"), WxCode=num("weathercode"), MSLP=num("mslp"), Rain24=num("last24hrsrainfall"))
  d$Obs_utc <- lv_parse_utc(d$Date,d$Time_raw)
  d$Temp_ok <- is.finite(d$Temp) & d$Temp>=-5 & d$Temp<=55
  d$RH_ok <- is.finite(d$RH) & d$RH>=0 & d$RH<=100
  d <- d |> arrange(desc(Obs_utc)) |> distinct(Station,Obs_utc,.keep_all=TRUE)
  newest <- suppressWarnings(max(d$Obs_utc,na.rm=TRUE)); if(!is.finite(newest)) newest <- as.POSIXct(NA)
  list(data=d, meta=list(obs_time=newest), audit=list(rows=nrow(d), temp_invalid=sum(!d$Temp_ok), rh_invalid=sum(!d$RH_ok), ts_invalid=sum(is.na(d$Obs_utc))))
}
fetch_imd_warn <- function() {
  x <- imd_df(imd_get_json("districtwarning")); names(x) <- lv_norm(names(x))
  need <- c("district","date","day1","day1color")
  if(!all(need %in% names(x))) stop(paste("Unexpected IMD warning fields; missing:",paste(setdiff(need,names(x)),collapse=", ")))
  x <- x[grepl(live_cfg$imd_district_pattern,trimws(x$district),ignore.case=TRUE),]
  if(!nrow(x)) stop("District not found in the IMD district warnings (adjust ECOSHIELD_IMD_DISTRICT_PATTERN).")
  x <- x[1,]
  utc <- if("utc" %in% names(x)) as.character(x$utc) else NA_character_
  long <- dplyr::bind_rows(lapply(1:5,function(k) { cc <- paste0("day",k)
  tibble(District=trimws(as.character(x$district)), Issue_date=as.character(x$date), Issue_utc_raw=utc, Day=k,
         Codes=if(cc %in% names(x)) as.character(x[[cc]]) else NA_character_,
         Color=suppressWarnings(as.integer(if(paste0(cc,"color") %in% names(x)) as.character(x[[paste0(cc,"color")]]) else NA_character_))) }))
  long$Description <- vapply(long$Codes,lv_warn_desc,character(1))
  long$Level <- ifelse(is.na(long$Color),"Unavailable",unname(LV_WARN_LEVEL[as.character(long$Color)]))
  issued <- lv_parse_utc(x$date,utc[1]); if(is.na(issued)) issued <- as.POSIXct(paste(x$date,"00:00"),format="%Y-%m-%d %H:%M",tz="UTC")
  list(data=long, meta=list(obs_time=issued), audit=list(rows=nrow(long), bad_color=sum(is.na(long$Color))))
}

# ---- SHARED STATE (one cache for all sessions, so extra visitors do not add requests) ----
lv_empty_archive <- function() tibble(Obs_end=as.POSIXct(character(),tz=LIVE_TZ), Period=character(), District_raw=character(), Station=character(), Network=character(), Rainfall_mm=numeric())
live_archive_path <- file.path(live_cfg$cache_dir,"tnsmart_daily_archive.csv")
live_archive_load <- function() {
  if(!file.exists(live_archive_path)) return(lv_empty_archive())
  x <- tryCatch(read_csv(live_archive_path,show_col_types=FALSE,col_types=cols(.default=col_character())),error=function(e) NULL)
  if(is.null(x) || !all(c("Obs_end","Period","District_raw","Station","Network","Rainfall_mm") %in% names(x))) return(lv_empty_archive())
  x |> mutate(Obs_end=as.POSIXct(Obs_end,tz=LIVE_TZ), Rainfall_mm=suppressWarnings(as.numeric(Rainfall_mm))) |> filter(!is.na(Obs_end),is.finite(Rainfall_mm))
}
lv_src0 <- function(enabled) list(enabled=enabled, data=NULL, meta=list(obs_time=as.POSIXct(NA)), audit=list(), error=NA_character_, last_ok=as.POSIXct(NA), last_attempt=as.POSIXct(NA), recovered_at=as.POSIXct(NA))
LIVE <- new.env()
LIVE$rain <- lv_src0(live_cfg$tn_enabled); LIVE$wx <- lv_src0(live_cfg$imd_enabled); LIVE$warn <- lv_src0(live_cfg$imd_enabled)
LIVE$archive <- tryCatch(live_archive_load(),error=function(e) lv_empty_archive())
LIVE$last_attempt <- as.POSIXct(NA); LIVE$ver <- 0L

live_update_src <- function(src, enabled, fn, now, label="source") {
  src$enabled <- enabled; if(!enabled) return(src)
  src$last_attempt <- now
  had_error <- !is.null(src$error) && !is.na(src$error)
  res <- tryCatch(fn(),error=function(e) e)
  if(inherits(res,"error")) { src$error <- paste(conditionMessage(res),collapse=" ")   # last valid data is kept (and labelled stale in the UI)
  message(sprintf("[EcoShield %s] WARN %s refresh failed: %s",format(Sys.time(),"%Y-%m-%d %H:%M:%S"),label,substr(src$error,1,300))) }
  else { src$data <- res$data; src$meta <- res$meta; src$audit <- res$audit; src$error <- NA_character_; src$last_ok <- now
  if(had_error) { src$recovered_at <- now; message(sprintf("[EcoShield %s] INFO %s recovered after a failed refresh",format(Sys.time(),"%Y-%m-%d %H:%M:%S"),label)) } }
  src
}
live_archive_add <- function() {
  d <- LIVE$rain$data; if(is.null(d) || !nrow(d)) return(invisible())
  d <- d |> filter(Valid,!is.na(Obs_end)) |> transmute(Obs_end,Period,District_raw,Station,Network,Rainfall_mm)
  if(!nrow(d)) return(invisible())
  a <- dplyr::bind_rows(LIVE$archive,d) |> distinct(Obs_end,District_raw,Station,.keep_all=TRUE) |> arrange(Obs_end,Station)
  LIVE$archive <- a
  tryCatch({ dir.create(live_cfg$cache_dir,showWarnings=FALSE,recursive=TRUE)
    write_csv(mutate(a,Obs_end=format(Obs_end,"%Y-%m-%d %H:%M:%S",tz=LIVE_TZ)),live_archive_path) },error=function(e) NULL)
  invisible()
}
live_refresh <- function(force=FALSE) {
  now <- Sys.time()
  no_data <- live_cfg$tn_enabled && (is.null(LIVE$rain$data) || !nrow(LIVE$rain$data))   # nothing valid yet: retry after the minimum gap, not a full cycle
  gap <- if(force || no_data) live_cfg$min_gap_sec else live_cfg$refresh_min*60*0.9
  if(!is.na(LIVE$last_attempt) && as.numeric(difftime(now,LIVE$last_attempt,units="secs")) < gap) return(invisible(FALSE))
  LIVE$last_attempt <- now
  prev_ok <- LIVE$rain$last_ok
  LIVE$rain <- live_update_src(LIVE$rain,live_cfg$tn_enabled,fetch_tnsmart,now,"TN-SMART")
  LIVE$wx   <- live_update_src(LIVE$wx,live_cfg$imd_enabled,fetch_imd_wx,now,"IMD weather")
  LIVE$warn <- live_update_src(LIVE$warn,live_cfg$imd_enabled,fetch_imd_warn,now,"IMD warnings")
  if(!identical(prev_ok,LIVE$rain$last_ok)) live_archive_add()
  LIVE$ver <- LIVE$ver + 1L
  invisible(TRUE)
}
live_snapshot <- function() list(rain=LIVE$rain, wx=LIVE$wx, warn=LIVE$warn, archive=LIVE$archive, ver=LIVE$ver)

# ---- STATUS from real timestamps / source metadata (never from page refreshes) ----
lv_status <- function(src, kind=c("daily","obs","forecast"), now=Sys.time()) {
  kind <- match.arg(kind)
  if(!isTRUE(src$enabled)) return(list(label="Not enabled",cls="off"))
  if(is.null(src$data) || !nrow(src$data)) return(list(label=if(is.na(src$error)) "Awaiting first retrieval" else "Data unavailable",cls="bad"))
  obs <- src$meta$obs_time
  if(is.null(obs) || is.na(obs)) return(list(label="Data present, timestamp unavailable",cls="bad"))
  age <- as.numeric(difftime(now,obs,units="hours"))
  lab <- if(age < -0.5) "Timestamp is in the future (check source)"
  else if(kind=="forecast") { if(age>36) "Forecast · issued over 36 h ago (delayed)" else "Forecast" }
  else if(kind=="daily") { if(age<=live_cfg$fresh_daily_h) "Recently updated (daily report)" else "Delayed observation" }
  else { if(age<=live_cfg$fresh_obs_h) "Live / recently updated" else "Delayed observation" }
  cls <- if(grepl("Delayed|future",lab)) "warn" else "ok"
  if(!is.na(src$error)) { lab <- paste0(lab," · latest refresh failed, may be stale"); cls <- "warn" }
  list(label=lab,cls=cls)
}

# ---- Optional VERIFIED station coordinates (never guessed) ----
lv_key <- function(x) stringi::stri_trans_nfc(stringr::str_squish(as.character(x)))
lv_coords <- tibble()
if(nzchar(live_cfg$coords_file) && file.exists(live_cfg$coords_file)) {
  cc <- tryCatch(read_csv(live_cfg$coords_file,show_col_types=FALSE),error=function(e) NULL)
  if(!is.null(cc) && all(c("Station","Latitude","Longitude") %in% names(cc)))
    lv_coords <- cc |> transmute(Key=lv_key(Station), Latitude=suppressWarnings(as.numeric(Latitude)), Longitude=suppressWarnings(as.numeric(Longitude)),
                                 Coord_source=if("Coordinate_Source" %in% names(cc)) as.character(Coordinate_Source) else NA_character_) |>
      filter(is.finite(Latitude),is.finite(Longitude),between(Latitude,-90,90),between(Longitude,-180,180)) |> distinct(Key,.keep_all=TRUE)
}

# ---------------------------------------------------------------------
# SERVER
# ---------------------------------------------------------------------
server <- function(input, output, session) {
  observe({
    if(nrow(rain)>0) {
      updateSelectInput(session,"rain_station",choices=c("All stations",sort(unique(rain$Station))),selected=isolate(input$rain_station) %||% "All stations")
      yr <- range(year(rain$Date),na.rm=TRUE); updateSliderInput(session,"rain_years",min=yr[1],max=yr[2],value=yr)
      updateSliderInput(session,"ov_years",min=yr[1],max=yr[2],value=yr)
      updateSliderInput(session,"anom_ref",min=yr[1],max=yr[2],value=c(yr[1],min(yr[1]+19,yr[2])))
      updateSelectizeInput(session,"rain_compare",choices=sort(unique(rain$Station)),server=FALSE)
    }
    if(nrow(aq_x)>0) { updateSelectInput(session,"aq_station",choices=c("All stations",aq_choices$station),selected=isolate(input$aq_station) %||% "All stations"); updateSelectInput(session,"aq_pollutant_filter",choices=c("All pollutants",aq_choices$poll),selected="All pollutants"); updateSelectInput(session,"aq_category_filter",choices=c("All categories",aq_choices$cat),selected="All categories"); updateSliderInput(session,"aq_range",min=aq_rng[1],max=aq_rng[2],value=aq_rng) }
    if(nrow(parks)>0) { updateSelectInput(session,"park_zone",choices=c("All zones",sort(unique(parks$Zone))),selected=isolate(input$park_zone) %||% "All zones"); updateSelectInput(session,"park_ward",choices=c("All wards",sort(unique(parks$Ward))),selected="All wards") }
  })
  
  # ---- Filtered datasets ----
  rain_f <- reactive({req(nrow(rain)>0); x<-rain; if(!is.null(input$rain_station)&&input$rain_station!="All stations") x<-filter(x,Station==input$rain_station); if(!is.null(input$rain_years)) x<-filter(x,year(Date)>=input$rain_years[1],year(Date)<=input$rain_years[2]); if(!is.null(input$rain_min)&&is.finite(input$rain_min)) x<-filter(x,Rainfall>=input$rain_min); x})
  aq_f <- reactive({
    x <- aq_x |> filter(is.finite(AQI.Value))
    if(!nrow(x)) return(x)
    if(!is.null(input$aq_station) && input$aq_station!="All stations") x <- filter(x,Station==input$aq_station)
    if(!is.null(input$aq_range)) x <- filter(x,AQI.Value>=input$aq_range[1],AQI.Value<=input$aq_range[2])
    if(!is.null(input$aq_pollutant_filter) && input$aq_pollutant_filter!="All pollutants") x <- filter(x,AQI.Poll==input$aq_pollutant_filter)
    if(!is.null(input$aq_category_filter) && input$aq_category_filter!="All categories") x <- filter(x,AQI.Cat==input$aq_category_filter)
    x
  })
  observeEvent(input$aq_reset, {
    updateSelectInput(session,"aq_station",selected="All stations"); updateSelectInput(session,"aq_pollutant_filter",selected="All pollutants")
    updateSelectInput(session,"aq_category_filter",selected="All categories"); updateSliderInput(session,"aq_range",value=aq_rng)
  })
  parks_f <- reactive({req(nrow(parks)>0); x<-parks; if(!is.null(input$park_zone)&&input$park_zone!="All zones") x<-filter(x,Zone==input$park_zone); if(!is.null(input$park_ward)&&input$park_ward!="All wards") x<-filter(x,Ward==input$park_ward); if(!is.null(input$park_search)&&nzchar(trimws(input$park_search))) x<-filter(x,str_detect(str_to_lower(Park),fixed(str_to_lower(trimws(input$park_search))))); if(!is.null(input$park_sort)){if(input$park_sort=="Name A–Z")x<-arrange(x,Park);if(input$park_sort=="Name Z–A")x<-arrange(x,desc(Park));if(input$park_sort=="Zone")x<-arrange(x,Zone,Park);if(input$park_sort=="Ward")x<-arrange(x,Ward,Park)}; x})
  
  # New reactives (Rainfall page)
  min_cell <- reactive({ v <- input$rain_min_n; if(is.null(v)||!is.finite(v)||v<1) 3L else as.integer(v) })
  # Same filters as rain_f but with the station filter replaced by the compare selection (if any)
  rain_cmp_f <- reactive({
    req(nrow(rain)>0); sel <- input$rain_compare; if(!length(sel)) return(NULL)
    x <- filter(rain,Station %in% sel)
    if(!is.null(input$rain_years)) x<-filter(x,year(Date)>=input$rain_years[1],year(Date)<=input$rain_years[2])
    if(!is.null(input$rain_min)&&is.finite(input$rain_min)) x<-filter(x,Rainfall>=input$rain_min)
    x })
  rain_series <- reactive({ cmp <- rain_cmp_f(); if(!is.null(cmp)) list(df=cmp,by_station=TRUE) else list(df=rain_f(),by_station=FALSE) })
  rain_stats <- reactive(station_stats(rain_f()))
  rain_src_noyear <- reactive({ req(nrow(rain)>0); x<-rain; if(!is.null(input$rain_station)&&input$rain_station!="All stations") x<-filter(x,Station==input$rain_station); if(!is.null(input$rain_min)&&is.finite(input$rain_min)) x<-filter(x,Rainfall>=input$rain_min); x })
  ov_rain <- reactive({ req(nrow(rain)>0); x<-rain; if(!is.null(input$ov_years)) x<-filter(x,year(Date)>=input$ov_years[1],year(Date)<=input$ov_years[2]); x })
  rain_unit <- reactive(switch(input$rain_view %||% "Monthly trend","Monthly trend"="month","Quarterly average"="quarter","Annual average"="year","month"))
  rain_filter_desc <- reactive({
    c(paste0("Station filter: ",input$rain_station %||% "All stations"),
      paste0("Year range: ",paste(input$rain_years,collapse="-")),
      paste0("Minimum recorded value: ",input$rain_min %||% 0),
      paste0("Generated: ",format(Sys.time(),"%Y-%m-%d %H:%M")),
      "Rainfall units are unverified; statistics describe individual records, not cumulative totals.")
  })
  
  # =================== OVERVIEW ===================
  output$overview_status <- renderUI({
    chip <- function(ok,txt) div(class=paste("es-chip",if(!ok)"bad"),span(class="es-dot"),txt)
    tagList(
      chip(nrow(rain)>0, if(nrow(rain)) paste0("Rainfall: ",fmt_n(nrow(rain))," usable records (",min(year(rain$Date)),"–",max(year(rain$Date)),")") else "Rainfall file missing or expected columns unavailable"),
      chip(nrow(aq)>0, if(nrow(aq)) paste0("Air quality: ",fmt_n(nrow(aq))," records · year unverified") else "Air-quality file missing or expected columns unavailable"),
      chip(nrow(parks)>0, if(nrow(parks)) paste0("Parks: ",fmt_n(nrow(parks))," distinct records") else "Park file missing or expected columns unavailable"),
      chip(nrow(flood_pts)>0, if(nrow(flood_pts)) paste0("Flood KML: ",nrow(flood_pts)," points") else "Flood KML not loaded (needs sf and the KML file)"),
      div(class="es-chip",span(class="es-dot"),paste0("App data loaded: ",format(APP_LOADED,"%d %b %Y %H:%M")," (load time, not observation time)")))
  })
  
  output$overview_kpis <- renderUI({
    x <- if(nrow(rain)) ov_rain() else rain
    yrs <- input$ov_years
    cov <- if(nrow(rain) && !is.null(yrs)) month_coverage(x,yrs[1],yrs[2]) else NULL
    div(class="es-kpis",
        kpi("Rainfall records",if(nrow(rain)) fmt_n(nrow(x)) else "N/A",if(nrow(rain)) paste0("In selected period; ",fmt_n(nrow(rain))," usable in total. ",fmt_n(rain_audit$conflict_rows)," conflicting rows excluded.") else "No usable rainfall data"),
        kpi("Flood locations",if(nrow(flood_pts)) nrow(flood_pts) else "N/A","KML points; no depth or severity attributes"),
        kpi("AQI stations",if(nrow(aq)) n_distinct(aq$Station) else "N/A",if(nrow(aq)) paste0(fmt_n(nrow(aq))," records; year unverified") else "Supplied dataset"),
        kpi("Distinct park names",if(nrow(parks)) n_distinct(parks$Park) else "N/A","Inventory count, not green-cover area"),
        kpi("Selected period",if(!is.null(yrs)&&nrow(rain)) paste0(yrs[1],"–",yrs[2]) else "N/A",if(nrow(rain)) paste0("Data span ",min(year(rain$Date)),"–",max(year(rain$Date))) else ""),
        kpi("Month coverage",if(!is.null(cov)) paste0(round(cov$pct,1),"%") else "N/A",if(!is.null(cov)) paste0(cov$present," of ",cov$total," calendar months have ≥1 usable record") else "Share of calendar months with any record"))
  })
  
  output$overview_map <- renderLeaflet({
    validate(need(have_sf,"Install sf to read the flood KML: install.packages('sf')."),need(nrow(flood_pts)>0,"KML not loaded or contains no mappable locations."))
    tile_opts <- providerTileOptions(errorTileUrl="data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7")
    pop <- vapply(seq_len(nrow(flood_pts)),function(i){
      r <- flood_pts[i,]; nm <- r$Name; ds <- r$Description
      paste0("<b>Recorded point ",r$Record,"</b>",
             if(!is.na(nm)&&nzchar(trimws(nm))) paste0("<br>Name: ",htmlEscape(nm)) else "",
             if(!is.na(ds)&&nzchar(trimws(ds))) paste0("<br>Description: ",htmlEscape(substr(ds,1,200))) else "",
             "<br>Latitude: ",round(r$Latitude,6),"<br>Longitude: ",round(r$Longitude,6),
             "<br><i>Historical recorded location. Depth, severity and current flood status are not available.</i>")
    },character(1))
    leaflet(flood_pts,options=leafletOptions(minZoom=8)) |>
      addProviderTiles(providers$OpenStreetMap.Mapnik,group="OpenStreetMap",options=tile_opts) |>
      addProviderTiles(providers$CartoDB.Positron,group="Carto light",options=tile_opts) |>
      addCircleMarkers(lng=~Longitude,lat=~Latitude,layerId=~Record,radius=6,color="#c4552d",fillColor="#ff9a76",fillOpacity=.85,weight=1.2,
                       clusterOptions=markerClusterOptions(),popup=pop,group="Flood-inundation points (KML)") |>
      addLayersControl(baseGroups=c("OpenStreetMap","Carto light"),overlayGroups="Flood-inundation points (KML)",options=layersControlOptions(collapsed=FALSE)) |>
      addScaleBar(position="bottomleft") |>
      addLegend(position="bottomright",colors="#ff9a76",labels="Recorded flood-inundation location (KML point)",title="Layers",opacity=.9) |>
      fitBounds(min(flood_pts$Longitude),min(flood_pts$Latitude),max(flood_pts$Longitude),max(flood_pts$Latitude))
  })
  ov_selected <- reactiveVal(NULL)
  observeEvent(input$overview_map_marker_click,{ click<-input$overview_map_marker_click; if(!is.null(click$id)) ov_selected(flood_pts|>filter(Record==as.integer(click$id))) })
  output$ov_map_selected <- renderUI({
    x <- ov_selected(); if(is.null(x)||!nrow(x)) return(div(class="es-note","Click a marker for its coordinates. Flood points cannot be linked to rainfall or AQI charts because those datasets have no verified coordinates."))
    div(class="es-note",tags$b(paste("Selected: recorded point",x$Record[1])),paste0(" · lat ",round(x$Latitude[1],5),", lon ",round(x$Longitude[1],5),". Historical point only; not a live flood status."))
  })
  output$ov_layer_status <- renderUI({
    on <- function(txt) span(txt); off <- function(txt) span(class="off",txt)
    out <- div(class="es-layer",
               if(nrow(flood_pts)) on(paste0("✔ Flood points plotted: ",nrow(flood_pts))) else off("✘ Flood points: KML unavailable"),
               off(paste0("○ Rainfall stations (",if(nrow(rain)) n_distinct(rain$Station) else 0,") not plotted: no verified coordinates")),
               off(paste0("○ AQI stations (",if(nrow(aq)) n_distinct(aq$Station) else 0,") not plotted: no verified coordinates")),
               off(paste0("○ Parks (",if(nrow(parks)) n_distinct(parks$Park) else 0,") not plotted: no coordinates in dataset")))
    tagList(out,if(nrow(flood_pts)) div(class="es-note",paste0("Coordinates read from KML and converted to WGS84 (source CRS: ",ifelse(is.na(flood_crs_in),"not reported",flood_crs_in),"). ",
                                                               if(flood_outside>0) paste0(flood_outside," point(s) fall outside a rough Chennai-region bounding box and should be checked. ") else "All points fall inside a rough Chennai-region bounding box. ",
                                                               "Basemap: © OpenStreetMap contributors / © CARTO. If tiles fail to load, the point layer still works.")))
  })
  
  output$overview_rain <- renderPlotly({
    validate(need(nrow(rain)>0,"Load a rainfall CSV in data/."))
    x <- ov_rain(); validate(need(nrow(x)>0,"No rainfall records in the selected period."))
    ts <- ts_summarise(x,input$ov_unit %||% "month")
    k <- as.integer(input$ov_k %||% 12)
    build_ts_plot(ts,input$ov_unit %||% "month",stat="Mean",roll=isTRUE(input$ov_roll),k=k)
  })
  output$ov_ts_note <- renderUI({
    u <- unit_word(input$ov_unit %||% "month")
    div(class="es-note",paste0("Solid line: mean of all usable records per ",u,", all stations pooled (station mix can vary over time; unit unverified). ",
                               if(isTRUE(input$ov_roll)) paste0("Dashed line: ",input$ov_k %||% 12,"-",u," trailing rolling mean of the plotted values (needs a full window and ≥ half non-missing periods). ") else "",
                               "These are averages of recorded values, not cumulative rainfall."))
  })
  
  output$overview_heat <- renderPlotly({
    validate(need(nrow(rain)>0,"Load a rainfall CSV in data/."))
    x <- ov_rain(); validate(need(nrow(x)>0,"No rainfall records in the selected period."))
    cells <- x |> mutate(Year=year(Date),Month=month(Date)) |> group_by(Year,Month) |> summarise(Val=mean(Rainfall),N=n(),.groups="drop")
    heat_plot(cells,"Mean of records",list(c(0,"#eef7f4"),c(.5,"#5fb7a1"),c(1,"#0d4f41")),min_n=OV_MIN_N)
  })
  
  output$overview_aqi <- renderPlotly({
    validate(need(nrow(aq)>0,"Load an air-quality CSV in data/."))
    x <- aq |> filter(!is.na(AQI.Value)) |> group_by(Station) |>
      summarise(Mean=mean(AQI.Value),Median=median(AQI.Value),Q1=quantile(AQI.Value,.25,names=FALSE),Q3=quantile(AQI.Value,.75,names=FALSE),N=n(),.groups="drop") |> arrange(Mean)
    validate(need(nrow(x)>0,"No numeric AQI values."))
    x$Station <- factor(x$Station,levels=x$Station)
    tip <- paste0("Station: ",x$Station,"<br>Mean AQI: ",round(x$Mean,1),"<br>Median: ",round(x$Median,1),"<br>IQR: ",round(x$Q1,1),"–",round(x$Q3,1),"<br>Records: ",x$N)
    plot_ly(x) |> add_segments(x=~Q1,xend=~Q3,y=~Station,yend=~Station,line=list(color="#a9d6ca",width=9),hoverinfo="none",name="IQR") |>
      add_markers(x=~Mean,y=~Station,text=tip,hoverinfo="text",marker=list(color="#14866d",size=11),name="Mean") |>
      add_markers(x=~Median,y=~Station,text=tip,hoverinfo="text",marker=list(color="#e2a33b",size=9,symbol="diamond"),name="Median") |>
      base_layout(xaxis=list(title="Recorded AQI value",gridcolor="#e8f0ed"),yaxis=list(title=""),legend=list(orientation="h",y=-0.2)) |> cfg()
  })
  
  output$overview_insight <- renderUI({
    x <- if(nrow(rain)) ov_rain() else rain; yrs <- input$ov_years
    rain_part <- if(nrow(rain)==0) list(insight_item("Rainfall","Rainfall data not loaded."))
    else if(!nrow(x)) list(insight_item("Rainfall","No usable rainfall records in the selected period."))
    else {
      m <- x |> group_by(Station) |> summarise(avg=mean(Rainfall),n=n(),.groups="drop")
      top <- slice_max(m,avg,n=1,with_ties=FALSE); low <- slice_min(m,avg,n=1,with_ties=FALSE)
      mon <- ts_summarise(x,"month",fill=FALSE) |> filter(N>=OV_MIN_N)
      cov <- month_coverage(x,yrs[1],yrs[2])
      out <- list(
        insight_item("Records",paste0(fmt_n(nrow(x))," usable records from ",n_distinct(x$Station)," stations, ",format(min(x$Date),"%d %b %Y")," to ",format(max(x$Date),"%d %b %Y"),".")),
        insight_item("Stations",if(nrow(m)>1) paste0("Highest station average of recorded values: ",top$Station," (",fmt_num(top$avg)," from ",fmt_n(top$n)," records). Lowest: ",low$Station," (",fmt_num(low$avg)," from ",fmt_n(low$n)," records). Averages are of individual records, not totals; stations with different record counts are not strictly comparable.") else paste0("Only one station in range: ",top$Station,".")))
      if(nrow(mon)) { hm <- slice_max(mon,Mean,n=1,with_ties=FALSE)
      out <- c(out,list(insight_item("Peak month",paste0("Highest monthly mean of records (months with ≥",OV_MIN_N," records): ",format(hm$Period,"%b %Y")," = ",fmt_num(hm$Mean)," from ",fmt_n(hm$N)," records at ",hm$Stations," station(s). Pooled means depend on which stations reported that month.")))) }
      warns <- c(if(max(m$n)/max(min(m$n),1)>2) paste0("Station record counts range from ",fmt_n(min(m$n))," to ",fmt_n(max(m$n)),"."),
                 if(sum(m$n<LOW_N)>0) paste0(sum(m$n<LOW_N)," station(s) have fewer than ",LOW_N," records in this period."),
                 if(cov$pct<100) paste0(round(100-cov$pct,1),"% of calendar months in this period have no usable record (missing, not zero)."),
                 if(mean(x$Rainfall==0)>0) paste0(round(100*mean(x$Rainfall==0),1),"% of records are recorded zeros."))
      c(out,list(insight_item("Data quality",if(length(warns)) paste(warns,collapse=" ") else "No coverage warnings for this period.")))
    }
    aq_part <- if(nrow(aq)==0) list(insight_item("AQI","Air-quality data not loaded.")) else {
      a <- filter(aq,!is.na(AQI.Value))
      if(!nrow(a)) list(insight_item("AQI","No numeric AQI values.")) else {
        am <- a |> group_by(Station) |> summarise(avg=mean(AQI.Value),mx=max(AQI.Value),n=n(),.groups="drop"); ta <- slice_max(am,avg,n=1,with_ties=FALSE)
        list(insight_item("AQI",paste0("Highest mean recorded AQI: ",ta$Station," (",fmt_num(ta$avg,1)," over ",ta$n," records; maximum single record ",fmt_num(ta$mx,0),"). Aggregation: mean of all supplied records per station. The observation year is unverified, so this is not a current AQI.")))
      } }
    tagList(div(class="es-insight-group","Rainfall (selected period)"),rain_part,
            div(class="es-insight-group","Air quality (all supplied records)"),aq_part,
            div(class="es-insight-group","Scope"),
            insight_item("Limits","Rainfall, AQI, flood and park datasets have no shared verified dates or coordinates, so no cross-dataset correlation or spatial relationship is calculated. No risk classes are assigned."))
  })
  
  quality_table <- reactive({
    ra <- rain_audit
    tibble(
      Dataset=c(rep("Rainfall",8),rep("Air quality",3),rep("Flood KML",3),rep("Parks",2)),
      Check=c("Raw rows read","Rows failing validity checks (excluded)","Exact duplicates removed","Rows in conflicting station-date groups (excluded)","Usable rows","Usable rows with recorded value 0","Stations","Date span",
              "Records","Records with numeric AQI","Stations","Points loaded","Points outside rough Chennai bounding box","Source CRS","Records","Distinct park names"),
      Value=c(fmt_n(ra$raw),fmt_n(ra$invalid),fmt_n(ra$dups),paste0(fmt_n(ra$conflict_rows)," (",fmt_n(ra$conflict_groups)," groups)"),fmt_n(ra$final),
              paste0(fmt_n(ra$zeros)," (",if(ra$final>0) round(100*ra$zeros/ra$final,1) else 0,"%)"),if(nrow(rain)) as.character(n_distinct(rain$Station)) else "0",
              if(nrow(rain)) paste(format(min(rain$Date)),"to",format(max(rain$Date))) else "N/A",
              fmt_n(nrow(aq)),fmt_n(sum(!is.na(aq$AQI.Value))),if(nrow(aq)) as.character(n_distinct(aq$Station)) else "0",
              as.character(nrow(flood_pts)),as.character(flood_outside),ifelse(is.na(flood_crs_in),"N/A",flood_crs_in),
              fmt_n(nrow(parks)),if(nrow(parks)) as.character(n_distinct(parks$Park)) else "0"),
      Note=c("Before cleaning","Blank station, unparseable date or non-numeric value; never treated as zero","Identical rows","Same station and date with different values; all rows excluded","After cleaning","Kept as recorded zeros; cannot be distinguished from placeholder zeros","","",
             "As supplied","Others excluded from AQI statistics","","KML points, converted to WGS84","Check only; points are kept","","Distinct Zone/Ward/Park combinations","Not an area measure"))
  })
  output$overview_quality <- renderDT(datatable(quality_table(),rownames=FALSE,options=list(dom="t",pageLength=20,scrollX=TRUE,ordering=FALSE)))
  output$quality_notes <- renderUI(tags$ul(tags$li(paste(nrow(rain_conflicts),"rainfall rows belong to conflicting station-date groups and are excluded.")),tags$li("Historical rainfall units are unverified."),tags$li("Air-quality observation year is unknown; comparisons are descriptive only."),tags$li("Flood points are not live conditions; no depth or severity is inferred."),tags$li("Park data contains no coordinates or area measurements."),tags$li("Missing rainfall observations are shown as gaps, never as zero.")))
  
  # =================== RAINFALL: KPIs ===================
  output$rain_kpis <- renderUI({
    x <- rain_f(); yrs <- input$rain_years
    if(!nrow(x)) return(div(class="es-kpis",kpi("Records","0","No records match the filters")))
    cov <- month_coverage(x,yrs[1],yrs[2])
    div(class="es-kpis",
        kpi("Usable records",fmt_n(nrow(x)),"After current filters"),
        kpi("Stations",n_distinct(x$Station),paste0(sum(rain_stats()$Low_coverage)," with <",LOW_N," records")),
        kpi("Period",paste0(yrs[1],"–",yrs[2]),paste0(format(min(x$Date),"%d %b %Y")," to ",format(max(x$Date),"%d %b %Y"))),
        kpi("Mean of records",fmt_num(mean(x$Rainfall)),"Unit unverified; not a total"),
        kpi("Median of records",fmt_num(median(x$Rainfall)),paste0(round(100*mean(x$Rainfall==0),1),"% recorded zeros")),
        kpi("Month coverage",paste0(round(cov$pct,1),"%"),paste0(cov$present," of ",cov$total," months have ≥1 record")))
  })
  
  # =================== RAINFALL: time series ===================
  rain_cp <- reactive({ cp_detect(ts_summarise(rain_f(),"month",fill=FALSE),min_cell()) })
  output$rain_trend <- renderPlotly({
    d <- rain_series(); x <- d$df; validate(need(nrow(x)>0,"No rainfall records match these filters."))
    unit <- rain_unit()
    ts <- ts_summarise(x,unit,by_station=d$by_station)
    cps <- if(isTRUE(input$rain_cp) && unit=="month" && !d$by_station) rain_cp()$dates else NULL
    build_ts_plot(ts,unit,stat=input$rain_stat %||% "Mean",roll=isTRUE(input$rain_roll),k=as.integer(input$rain_k %||% 12),cps=cps,slider=TRUE)
  })
  output$rain_cp_note <- renderUI({
    d <- rain_series(); unit <- rain_unit()
    base <- div(class="es-note",paste0("Solid line = ",if((input$rain_stat %||% "Mean")=="Median") "median" else "mean"," of individual records per ",unit_word(unit),
                                       if(d$by_station) " for each compared station" else paste0(" for ",if((input$rain_station %||% "All stations")=="All stations") "all stations pooled" else input$rain_station),
                                       "; gaps = no usable records (not zero). Not a cumulative total; unit unverified."))
    if(!isTRUE(input$rain_cp)) return(base)
    if(unit!="month" || d$by_station) return(tagList(base,div(class="notice warning es-note","Change-point detection runs only for the monthly view with a single series (clear the station comparison).")))
    cp <- rain_cp()
    if(!cp$ok) return(tagList(base,div(class="notice warning es-note",cp$msg)))
    res <- if(is.null(cp$segs)) div(class="notice info es-note",cp$msg) else
      div(class="notice info es-note",tags$b("Detected statistical mean shifts (start of each new segment): "),paste(format(cp$dates,"%b %Y"),collapse=", "),
          tags$br(),"Segments (months / mean z): ",paste0(format(cp$segs$Start,"%b %Y"),"–",format(cp$segs$End,"%b %Y")," (",cp$segs$Months," mo, z=",round(cp$segs$`Mean z`,2),")",collapse="; "),".")
    tagList(base,res,div(class="es-note","Method: PELT mean-shift on deseasonalised monthly means (z by calendar month), MBIC penalty, minimum 12-month segments. Exploratory; these are statistical shifts, not evidence of climate change, floods or any cause."))
  })
  
  # =================== RAINFALL: heat map ===================
  output$rain_heat <- renderPlotly({
    x <- rain_f(); validate(need(nrow(x)>0,"No rainfall records match these filters."))
    cells <- x |> mutate(Year=year(Date),Month=month(Date)) |> group_by(Year,Month) |> summarise(Mean=mean(Rainfall),Median=median(Rainfall),N=n(),.groups="drop")
    st <- input$heat_stat %||% "Mean of records"
    if(st=="Number of records") {
      cells$Val <- cells$N
      heat_plot(cells,"Records",list(c(0,"#eef4f8"),c(1,"#2c5f7c")),min_n=1,valfmt=function(v) fmt_n(v))
    } else {
      cells$Val <- if(st=="Median of records") cells$Median else cells$Mean
      heat_plot(cells,st,list(c(0,"#eef7f4"),c(.5,"#5fb7a1"),c(1,"#0d4f41")),min_n=min_cell())
    }
  })
  
  # =================== RAINFALL: seasonal profile ===================
  season_data <- reactive({
    d <- rain_series(); x <- d$df
    basis <- input$season_basis %||% "records"
    if(basis=="yearly") {
      b <- x |> mutate(Year=year(Date),Month=month(Date)) |> group_by(Station,Year,Month) |> summarise(V=mean(Rainfall),n=n(),.groups="drop") |> filter(n>=min_cell())
      if(!d$by_station) b <- b |> group_by(Year,Month) |> summarise(V=mean(V),.groups="drop") |> mutate(Station="All selected stations")
    } else {
      b <- x |> transmute(Station=if(d$by_station) Station else "All selected stations",Month=month(Date),V=Rainfall)
    }
    if(!nrow(b)) return(tibble())
    if(!"Month" %in% names(b)) return(tibble())
    b |> group_by(Station,Month) |> summarise(Mean=mean(V),Median=median(V),Q1=quantile(V,.25,names=FALSE),Q3=quantile(V,.75,names=FALSE),N=n(),.groups="drop") |> arrange(Station,Month)
  })
  output$rain_season <- renderPlotly({
    s <- season_data(); validate(need(nrow(s)>0,"No rainfall records match these filters (or too few records per month cell)."))
    basis <- input$season_basis %||% "records"; nlab <- if(basis=="yearly") "Years" else "Records"
    stns <- unique(s$Station); pal <- station_palette(length(stns)); single <- length(stns)==1
    tipf <- function(d) paste0(d$Station,"<br>",month.name[d$Month],"<br>Median: ",fmt_num(d$Median),"<br>Mean: ",fmt_num(d$Mean),"<br>IQR: ",fmt_num(d$Q1),"–",fmt_num(d$Q3),"<br>",nlab,": ",fmt_n(d$N))
    if(isTRUE(input$season_polar)) {
      p <- plot_ly(type="scatterpolar",mode="lines")
      for(i in seq_along(stns)) { d <- s[s$Station==stns[i],]; th <- month.abb[d$Month]
      if(single && nrow(d)>1) { iq <- c(d$Q3,rev(d$Q1)); p <- add_trace(p,r=c(iq,iq[1]),theta=c(month.abb[c(d$Month,rev(d$Month))],month.abb[d$Month[1]]),mode="lines",fill="toself",fillcolor="rgba(20,134,109,0.15)",line=list(width=0),hoverinfo="skip",name="IQR") }
      p <- add_trace(p,r=c(d$Median,d$Median[1]),theta=c(th,th[1]),mode="lines+markers",name=paste0(stns[i]," (median)"),text=c(tipf(d),tipf(d)[1]),hoverinfo="text",line=list(color=pal[i],width=2.5),marker=list(color=pal[i],size=5)) }
      return(p |> base_layout(polar=list(angularaxis=list(direction="clockwise",categoryarray=month.abb,categoryorder="array"),radialaxis=list(rangemode="tozero",gridcolor="#e8f0ed")),legend=list(orientation="h",y=-0.1),margin=list(l=40,r=40,t=30,b=40)) |> cfg())
    }
    p <- plot_ly()
    for(i in seq_along(stns)) { d <- s[s$Station==stns[i],]
    if(single && nrow(d)>1) p <- add_trace(p,x=c(d$Month,rev(d$Month)),y=c(d$Q3,rev(d$Q1)),type="scatter",mode="lines",fill="toself",fillcolor="rgba(20,134,109,0.15)",line=list(width=0),hoverinfo="skip",name="IQR (Q1–Q3)")
    p <- add_trace(p,x=d$Month,y=d$Median,type="scatter",mode="lines+markers",name=paste0(stns[i]," – median"),text=tipf(d),hoverinfo="text",line=list(color=pal[i],width=2.5),marker=list(color=pal[i],size=6))
    if(single) p <- add_trace(p,x=d$Month,y=d$Mean,type="scatter",mode="markers",name="Mean",text=tipf(d),hoverinfo="text",marker=list(color="#e2a33b",symbol="diamond",size=8))
    }
    p |> base_layout(xaxis=list(title="Calendar month",tickmode="array",tickvals=1:12,ticktext=month.abb,showgrid=FALSE),
                     yaxis=list(title="Recorded value (unit unverified)",rangemode="tozero",gridcolor="#e8f0ed"),legend=list(orientation="h",y=-0.22)) |> cfg()
  })
  output$season_note <- renderUI({
    s <- season_data(); if(!nrow(s)) return(NULL)
    basis <- input$season_basis %||% "records"
    stns <- unique(s$Station)
    div(class="es-note",
        if(basis=="yearly") paste0("Spread = across yearly monthly means (n = years with ≥",min_cell()," records in that month; range ",min(s$N)," to ",max(s$N),"). ") else paste0("Spread = across individual records (n per month: ",fmt_n(min(s$N))," to ",fmt_n(max(s$N)),"). "),
        "This is a profile of typical recorded values, not seasonal totals.",
        if(length(stns)>1) " Compared stations may have different observation coverage; check the counts in the hover text before comparing them." else "",
        if(basis=="records" && any(s$Median==0)) " Median is 0 in some months because at least half of the records there are zeros." else "")
  })
  
  # =================== RAINFALL: anomalies ===================
  rain_anom <- reactive({
    req(input$anom_ref,input$rain_years)
    anomaly_calc(rain_src_noyear(),input$anom_ref,input$rain_years,min_cell())
  })
  output$anom_note <- renderUI({
    a <- rain_anom(); nb <- nrow(a$base)
    method <- div(class="es-note",paste0("Baseline: calendar-month mean and SD of yearly monthly means over ",input$anom_ref[1],"–",input$anom_ref[2],
                                         " (requires ≥",MIN_REF_YEARS," reference years per month and ≥",min_cell()," records per month cell). z = (month mean − baseline mean) ÷ baseline SD; deviation = month mean − baseline mean. Compared with the same calendar month only. Station filter and minimum recorded value apply; displayed years follow the year-range filter."))
    if(nb==0) return(tagList(method,div(class="notice warning es-note",tags$b("Anomaly analysis disabled: "),"no calendar month has enough reference-period data for a baseline. Widen the reference period, choose a station with longer coverage or lower the minimum records per month cell.")))
    tagList(method,div(class="es-note",paste0(nb," of 12 calendar months have a usable baseline; other months are blank. Rainfall is skewed, so z-scores are descriptive indices (not probabilities) and large values are statistical anomalies, not confirmed floods, droughts or climate evidence.")))
  })
  output$anom_heat <- renderPlotly({
    a <- rain_anom(); validate(need(nrow(a$base)>0,"Anomaly analysis disabled: insufficient reference data."),need(nrow(a$data)>0,"No months with baseline in the selected years."))
    d <- a$data; use_z <- (input$anom_method %||% "z")=="z"
    d$Val <- if(use_z) d$Z else d$Dev
    lim <- max(abs(d$Val),na.rm=TRUE); if(!is.finite(lim)||lim==0) lim <- 1
    heat_plot(d,if(use_z) "z-score" else "Deviation (unit unverified)",list(c(0,"#a6611a"),c(.5,"#f7f7f7"),c(1,"#14866d")),min_n=1,zmin=-lim,zmax=lim,valfmt=function(v) fmt_num(v,2))
  })
  output$anom_table <- renderDT({
    a <- rain_anom(); validate(need(nrow(a$data)>0,"No anomaly data."))
    t <- a$data |> arrange(desc(abs(Z))) |> slice_head(n=10) |> transmute(Year,Month=month.abb[Month],`Month mean`=Mean,`Baseline mean`=BaseMean,Deviation=Dev,`z`=Z,Records=N)
    datatable(t,rownames=FALSE,options=list(dom="t",pageLength=10,scrollX=TRUE,ordering=FALSE)) |> formatRound(c("Month mean","Baseline mean","Deviation","z"),2)
  })
  
  # =================== RAINFALL: station comparison ===================
  output$rain_stations <- renderPlotly({
    st <- rain_stats(); validate(need(nrow(st)>0,"No station data."))
    x <- st |> slice_max(Mean,n=15,with_ties=FALSE) |> arrange(Mean); x$Station <- factor(x$Station,levels=x$Station)
    tip <- paste0("Station: ",x$Station,"<br>Mean: ",fmt_num(x$Mean),"<br>Median: ",fmt_num(x$Median),"<br>IQR: ",fmt_num(x$Q1),"–",fmt_num(x$Q3),"<br>SD: ",fmt_num(x$SD),"<br>Records: ",fmt_n(x$Records),ifelse(x$Low_coverage,"<br><b>Low coverage</b>",""))
    plot_ly(x) |> add_segments(x=~Q1,xend=~Q3,y=~Station,yend=~Station,line=list(color="#cfe5de",width=8),hoverinfo="none",name="IQR",showlegend=TRUE) |>
      add_markers(x=~Median,y=~Station,text=tip,hoverinfo="text",marker=list(color="#e2a33b",symbol="diamond",size=8),name="Median") |>
      add_markers(x=~Mean,y=~Station,text=tip,hoverinfo="text",name="Mean",marker=list(size=11,color=~Records,colorscale=list(c(0,"#c9e6dd"),c(1,"#0d4f41")),showscale=TRUE,colorbar=list(title=list(text="Records",font=list(size=11)),thickness=10,len=.7),line=list(color="#fff",width=1))) |>
      base_layout(xaxis=list(title="Recorded value (unit unverified)",gridcolor="#e8f0ed",rangemode="tozero"),yaxis=list(title=""),legend=list(orientation="h",y=-0.15)) |> cfg()
  })
  output$rain_bubble <- renderPlotly({
    x <- rain_stats(); validate(need(nrow(x)>0,"No station data for selected filters."))
    x$SD[is.na(x$SD)] <- 0
    tip <- paste0("Station: ",x$Station,"<br>Mean: ",fmt_num(x$Mean),"<br>SD: ",fmt_num(x$SD),"<br>Records: ",fmt_n(x$Records),"<br>Years with data: ",x$Years_with_data,ifelse(x$Low_coverage,"<br><b>Low coverage</b>",""))
    plot_ly(x,x=~Mean,y=~Records,type="scatter",mode="markers",text=tip,hoverinfo="text",
            marker=list(size=~Records,sizemode="area",sizeref=2*max(x$Records)/(34^2),sizemin=6,opacity=.8,color=~SD,colorscale=list(c(0,"#c9e6dd"),c(1,"#7a3b12")),showscale=TRUE,colorbar=list(title=list(text="SD",font=list(size=11)),thickness=10,len=.7),line=list(color="#fff",width=1))) |>
      base_layout(xaxis=list(title="Average recorded value (unit unverified)",gridcolor="#e8f0ed",rangemode="tozero"),yaxis=list(title="Record count",gridcolor="#e8f0ed",rangemode="tozero")) |> cfg()
  })
  
  # =================== RAINFALL: distribution ===================
  output$rain_distribution <- renderPlotly({
    x <- rain_f(); validate(need(nrow(x)>0,"No rainfall records."))
    view <- input$dist_view %||% "Histogram"; lg <- isTRUE(input$dist_log)
    tr <- function(v) if(lg) log1p(v) else v
    xlab <- if(lg) "log(1 + recorded value), unit unverified" else "Recorded value (unit unverified)"
    if(view=="Histogram") {
      h <- hist(tr(x$Rainfall),breaks=40,plot=FALSE)
      pq <- quantile(x$Rainfall,c(.5,.9,.95,.99),names=FALSE); lbl <- c("P50","P90","P95","P99")
      shp <- lapply(tr(pq),function(v) list(type="line",x0=v,x1=v,y0=0,y1=1,yref="paper",line=list(color="#c0583c",dash="dot",width=1.3)))
      ann <- lapply(seq_along(pq),function(i) list(x=tr(pq[i]),y=1-0.07*(i-1),yref="paper",text=paste0(lbl[i],"=",fmt_num(pq[i])),showarrow=FALSE,xanchor="left",font=list(size=10,color="#c0583c")))
      plot_ly(x=h$mids,y=h$counts,type="bar",marker=list(color="#1a9679"),
              text=paste0("Bin: ",round(h$breaks[-length(h$breaks)],2)," to ",round(h$breaks[-1],2),"<br>Records: ",fmt_n(h$counts)),hoverinfo="text") |>
        base_layout(xaxis=list(title=xlab),yaxis=list(title="Records",gridcolor="#e8f0ed"),shapes=shp,annotations=ann,bargap=0) |> cfg()
    } else if(view=="Empirical CDF") {
      pr <- seq(0,1,length.out=400); q <- quantile(x$Rainfall,pr,names=FALSE)
      plot_ly(x=tr(q),y=pr,type="scatter",mode="lines",line=list(color="#14866d",width=2,shape="hv"),
              text=paste0("Share of records ≤ ",fmt_num(q),": ",round(100*pr,1),"%"),hoverinfo="text") |>
        base_layout(xaxis=list(title=xlab),yaxis=list(title="Cumulative share of records",tickformat=".0%",gridcolor="#e8f0ed",range=c(0,1.02))) |> cfg()
    } else {
      st <- x |> count(Station,sort=TRUE) |> slice_head(n=10) |> pull(Station)
      b <- x |> filter(Station %in% st) |> group_by(Station) |>
        summarise(Q1=quantile(Rainfall,.25,names=FALSE),Med=median(Rainfall),Q3=quantile(Rainfall,.75,names=FALSE),Mn=mean(Rainfall),
                  lo=min(Rainfall[Rainfall>=quantile(Rainfall,.25,names=FALSE)-1.5*IQR(Rainfall)]),hi=max(Rainfall[Rainfall<=quantile(Rainfall,.75,names=FALSE)+1.5*IQR(Rainfall)]),N=n(),.groups="drop") |> arrange(N)
      plot_ly(type="box",orientation="h",y=b$Station,q1=tr(b$Q1),median=tr(b$Med),q3=tr(b$Q3),lowerfence=tr(b$lo),upperfence=tr(b$hi),mean=tr(b$Mn),
              marker=list(color="#14866d"),line=list(color="#14866d"),fillcolor="rgba(26,150,121,0.25)",
              text=paste0(b$Station,"<br>Median: ",fmt_num(b$Med),"<br>IQR: ",fmt_num(b$Q1),"–",fmt_num(b$Q3),"<br>Records: ",fmt_n(b$N)),hoverinfo="text") |>
        base_layout(xaxis=list(title=xlab,gridcolor="#e8f0ed"),yaxis=list(title="",categoryorder="array",categoryarray=b$Station),margin=list(l=110,r=20,t=20,b=45)) |> cfg()
    }
  })
  output$dist_note <- renderUI({
    x <- rain_f(); if(!nrow(x)) return(NULL)
    q <- quantile(x$Rainfall,c(.5,.9,.95,.99),names=FALSE)
    tagList(div(class="es-note",paste0("n = ",fmt_n(nrow(x))," records · recorded zeros: ",round(100*mean(x$Rainfall==0),1),"% · P50 ",fmt_num(q[1])," · P90 ",fmt_num(q[2])," · P95 ",fmt_num(q[3])," · P99 ",fmt_num(q[4]),
                                       if(isTRUE(input$dist_log)) ". Axis shows log(1 + value) so zeros remain visible; distances are multiplicative, not additive." else ".",
                                       if((input$dist_view %||% "Histogram")!="Histogram" && (input$dist_view %||% "")!="Empirical CDF") " Boxes show median/IQR with 1.5×IQR whiskers; outliers are not drawn. Statistical outliers are not hazard classifications." else "")),
            tags$details(class="es-method",tags$summary("Highest recorded values in view"),
                         { hv <- slice_max(x,Rainfall,n=8,with_ties=FALSE)
                         tags$table(class="table table-sm",tags$thead(tags$tr(tags$th("Station"),tags$th("Date"),tags$th("Recorded value"))),
                                    tags$tbody(lapply(seq_len(nrow(hv)),function(i) tags$tr(tags$td(hv$Station[i]),tags$td(format(hv$Date[i])),tags$td(fmt_num(hv$Rainfall[i])))))) }))
  })
  
  # =================== RAINFALL: insights ===================
  output$rain_insight <- renderUI({
    x <- rain_f(); if(!nrow(x)) return(p("No records match the current filters."))
    st <- rain_stats(); yrs <- input$rain_years; cov <- month_coverage(x,yrs[1],yrs[2])
    top <- slice_max(st,Mean,n=1,with_ties=FALSE); low <- slice_min(st,Mean,n=1,with_ties=FALSE)
    mx <- slice_max(x,Rainfall,n=1,with_ties=FALSE)
    multi <- nrow(st)>1
    obs <- if(multi) paste0("Highest station average of recorded values: ",top$Station," (",fmt_num(top$Mean)," from ",fmt_n(top$Records)," records). Lowest: ",low$Station," (",fmt_num(low$Mean)," from ",fmt_n(low$Records)," records).") else paste0("Single station in view: ",top$Station," (mean ",fmt_num(top$Mean),", median ",fmt_num(top$Median),").")
    ctx <- paste0(fmt_n(nrow(x))," usable records, ",nrow(st)," station(s), ",yrs[1],"–",yrs[2]," (data from ",format(min(x$Date),"%d %b %Y")," to ",format(max(x$Date),"%d %b %Y"),"). Highest single record: ",fmt_num(mx$Rainfall[1])," at ",mx$Station[1]," on ",format(mx$Date[1],"%d %b %Y"),".")
    warns <- c(if(multi && max(st$Records)/max(min(st$Records),1)>2) paste0("Station record counts differ by more than 2× (",fmt_n(min(st$Records))," to ",fmt_n(max(st$Records)),"), so differences in averages may reflect coverage."),
               if(any(st$Low_coverage)) paste0(sum(st$Low_coverage)," of ",nrow(st)," stations have fewer than ",LOW_N," records."),
               if(cov$pct<100) paste0(cov$total-cov$present," of ",cov$total," calendar months in the selected years have no usable record (missing, not zero)."),
               if(!is.null(input$rain_min) && is.finite(input$rain_min) && input$rain_min>0) paste0("Records below ",input$rain_min," are filtered out, which raises all averages."),
               if(rain_audit$conflict_rows>0) paste0(fmt_n(rain_audit$conflict_rows)," rows in conflicting station-date groups are excluded from all analyses."))
    div(class="notice info es-notice",style="margin:0",
        insight_item("Observation",obs),insight_item("Data context",ctx),
        insight_item("Interpretation",paste0("Mean of records = ",fmt_num(mean(x$Rainfall))," vs median = ",fmt_num(median(x$Rainfall)),
                                             if(mean(x$Rainfall)>1.5*median(x$Rainfall)+1e-9) ": the mean is much higher than the median, indicating a right-skewed distribution; the median or percentiles describe typical records better. " else ". ",
                                             "These are summaries of recorded values (",round(100*mean(x$Rainfall==0),1),"% are zeros), not rainfall totals.")),
        insight_item("Limitation",paste0(if(length(warns)) paste(warns,collapse=" ") else "No coverage warnings for this selection.",
                                         " Units are unverified and no environmental risk class is inferred.")))
  })
  
  # =================== RAINFALL: coverage ===================
  output$rain_cov_heat <- renderPlotly({
    x <- rain_f(); validate(need(nrow(x)>0,"No rainfall records match these filters."))
    top <- x |> count(Station,sort=TRUE) |> slice_head(n=35) |> pull(Station)
    c1 <- x |> filter(Station %in% top) |> mutate(Year=year(Date)) |> count(Station,Year,name="N")
    yrs <- seq(min(c1$Year),max(c1$Year)); stn <- rev(top)
    Z <- matrix(NA_real_,length(stn),length(yrs)); Z[cbind(match(c1$Station,stn),match(c1$Year,yrs))] <- c1$N
    TXT <- matrix(paste0(rep(stn,times=length(yrs)),", ",rep(yrs,each=length(stn)),"<br>",ifelse(is.na(Z),"No usable records",paste0("Records: ",fmt_n(Z)))),nrow=length(stn))
    plot_ly(x=yrs,y=stn,z=Z,type="heatmap",colorscale=list(c(0,"#eef4f8"),c(1,"#2c5f7c")),text=TXT,hoverinfo="text",xgap=1,ygap=1,colorbar=list(title=list(text="Records",font=list(size=11)),thickness=10,len=.8)) |>
      base_layout(xaxis=list(title="Year",showgrid=FALSE,dtick=if(length(yrs)>20) 2 else 1),yaxis=list(title="",showgrid=FALSE,tickfont=list(size=9),categoryorder="array",categoryarray=stn),margin=list(l=110,r=20,t=10,b=45)) |> cfg()
  })
  output$rain_cov_year <- renderPlotly({
    x <- rain_f(); validate(need(nrow(x)>0,"No rainfall records match these filters."))
    y <- x |> mutate(Year=year(Date)) |> group_by(Year) |> summarise(N=n(),Stations=n_distinct(Station),.groups="drop")
    plot_ly(y) |> add_bars(x=~Year,y=~N,name="Records",marker=list(color="#1a9679"),text=paste0(y$Year,"<br>Records: ",fmt_n(y$N),"<br>Stations reporting: ",y$Stations),hoverinfo="text") |>
      add_lines(x=~Year,y=~Stations,name="Stations reporting",yaxis="y2",line=list(color="#e2a33b",width=2),hoverinfo="skip") |>
      base_layout(xaxis=list(title="",dtick=if(nrow(y)>20) 2 else 1),yaxis=list(title="Records",gridcolor="#e8f0ed"),yaxis2=list(title="Stations",overlaying="y",side="right",showgrid=FALSE,rangemode="tozero"),
                  legend=list(orientation="h",y=-0.25),margin=list(l=55,r=45,t=10,b=45)) |> cfg()
  })
  quality_rain_tbl <- reactive({
    x <- rain_f(); yrs <- input$rain_years; ra <- rain_audit
    cov <- if(nrow(x)&&!is.null(yrs)) month_coverage(x,yrs[1],yrs[2]) else list(present=0,total=NA)
    tibble(Category=c("Zero recorded rainfall (valid, in view)","Missing months (no usable record, in view)","Invalid rows (excluded, whole dataset)","Exact duplicates removed (whole dataset)","Conflicting rows excluded (whole dataset)"),
           Count=c(sum(x$Rainfall==0),if(is.na(cov$total)) NA else cov$total-cov$present,ra$invalid,ra$dups,ra$conflict_rows),
           Note=c("Real recorded value of 0","Calendar months in the selected years with no record: not zero","Blank station, bad date or non-numeric value","Identical rows","Same station and date, different values"))
  })
  output$rain_audit_table <- renderDT(datatable(quality_rain_tbl(),rownames=FALSE,options=list(dom="t",pageLength=10,scrollX=TRUE,ordering=FALSE)))
  
  # =================== RAINFALL: tables & downloads ===================
  output$rain_stats_table <- renderDT({
    st <- rain_stats(); validate(need(nrow(st)>0,"No station statistics for current filters."))
    t <- st |> arrange(desc(Mean)) |> transmute(Station,Records,Mean,Median,SD,Q1,Q3,IQR,P90,Max,`% zeros`=100*Zero_share,`First date`=First_date,`Last date`=Last_date,`Years with data`=Years_with_data,`Low coverage`=ifelse(Low_coverage,"Yes","No"))
    datatable(t,rownames=FALSE,filter="top",options=list(pageLength=10,scrollX=TRUE)) |> formatRound(c("Mean","Median","SD","Q1","Q3","IQR","P90","Max","% zeros"),2)
  })
  output$rain_table <- renderDT(datatable(rain_f()|>arrange(desc(Date)),rownames=FALSE,filter="top",options=list(pageLength=10,scrollX=TRUE)))
  output$download_rain <- downloadHandler(filename=function()paste0("ecoshield_rainfall_",Sys.Date(),".csv"),content=function(file)write_csv(rain_f(),file))
  output$download_rain_stats <- downloadHandler(filename=function()paste0("ecoshield_rainfall_station_statistics_",Sys.Date(),".csv"),
                                                content=function(file) write_with_header(rain_stats(),file,c("EcoShield station statistics (individual records; not cumulative totals)",rain_filter_desc(),
                                                                                                             "Statistics: Mean, Median, SD, Q1, Q3, IQR, P90, Max computed over usable records per station; Low_coverage = fewer than 30 records.")))
  output$download_rain_quality <- downloadHandler(filename=function()paste0("ecoshield_rainfall_data_quality_",Sys.Date(),".csv"),
                                                  content=function(file) write_with_header(bind_rows(quality_table() |> filter(Dataset=="Rainfall") |> transmute(Category=Check,Count=Value,Note),
                                                                                                     quality_rain_tbl() |> mutate(Count=as.character(Count))),file,
                                                                                           c("EcoShield rainfall data-quality summary",rain_filter_desc(),paste0("Source file: ",ifelse(is.na(rain_path),"not found",basename(rain_path))),
                                                                                             "Cleaning: invalid rows removed; exact duplicates removed; station-date groups with conflicting values excluded; missing observations are never treated as zero.")))
  
  # =================== UNCHANGED PAGES (Flood Map, Parks, Action Plan; Air Quality outputs are in their own section further below) ===================
  # =================== FLOOD MAP (upgraded GIS page) ===================
  flood_sel <- reactiveVal(NULL)
  flood_f <- reactive({
    validate(need(flood_ok,"KML not loaded or contains no mappable locations."))
    x <- flood_valid
    if(!is.null(input$flood_id)) x <- filter(x,Record>=input$flood_id[1],Record<=input$flood_id[2])
    if(!is.null(input$flood_lat)) x <- filter(x,Latitude>=input$flood_lat[1],Latitude<=input$flood_lat[2])
    if(!is.null(input$flood_lon)) x <- filter(x,Longitude>=input$flood_lon[1],Longitude<=input$flood_lon[2])
    if(isTRUE(input$flood_attr_only)) x <- filter(x,Has_attr)
    x
  })
  flood_eps <- reactive(as.numeric(input$flood_eps %||% 500))
  flood_cell <- reactive(as.numeric(input$flood_cell %||% 1000))
  # Pairwise distances, nearest neighbours and proximity groups for the DISPLAYED points
  flood_prox <- reactive({
    x <- flood_f(); n <- nrow(x)
    if(n<2) return(NULL)
    if(n>1500) return(list(skipped=TRUE,n=n))
    D <- hav_matrix(x$Longitude,x$Latitude); diag(D) <- NA
    nn <- apply(D,1,min,na.rm=TRUE)
    Dg <- D; diag(Dg) <- Inf
    pg <- prox_groups(Dg,flood_eps())
    list(skipped=FALSE,x=x,D=D,nn=nn,group=pg$group,ngroups=pg$ngroups,eps=flood_eps())
  })
  flood_sel_row <- reactive({ s <- flood_sel(); if(is.null(s)) return(NULL); r <- filter(flood_valid,Record==s); if(nrow(r)) r else NULL })
  
  # Selection handling: marker click, table row, clear; selection is dropped when filtered out
  observeEvent(input$flood_map_marker_click,{
    id <- suppressWarnings(as.integer(input$flood_map_marker_click$id))
    if(length(id)==1 && !is.na(id) && id %in% flood_valid$Record) flood_sel(id)
  })
  observeEvent(input$flood_clear,flood_sel(NULL))
  observeEvent(flood_f(),{ s <- flood_sel(); if(!is.null(s) && !(s %in% flood_f()$Record)) flood_sel(NULL) })
  
  # ---- Header status, KPIs ----
  output$flood_status <- renderUI({
    chip <- function(ok,txt) div(class=paste("es-chip",if(!ok)"bad"),span(class="es-dot"),txt)
    tagList(
      chip(flood_ok,if(flood_ok) paste0("Dataset loaded: ",fmt_n(nrow(flood_geo))," geographic records (",fmt_n(nrow(flood_valid))," valid)") else if(!have_sf) "KML not loaded: install the 'sf' package" else "KML not found or contains no usable points"),
      chip(TRUE,"Recorded locations from the supplied KML; not live flood monitoring"),
      if(flood_ok) chip(TRUE,paste0("CRS: ",ifelse(is.na(flood_crs_in),"not reported","as reported by sf")," → WGS84 (EPSG:4326)")),
      if(flood_ok && (flood_outside>0 || flood_swapped>0)) chip(FALSE,if(flood_swapped>0) paste0(flood_swapped," point(s) look like swapped lon/lat: check source") else paste0(flood_outside," point(s) outside a rough Chennai-region box")))
  })
  output$flood_kpis <- renderUI({
    if(!flood_ok) return(div(class="es-kpis",kpi("Recorded points","N/A","KML not loaded"),kpi("Unique coordinates","N/A",""),kpi("Latitude range","N/A",""),kpi("Longitude range","N/A",""),kpi("Valid geometries","N/A",""),kpi("Missing descriptions","N/A","")))
    x <- flood_f(); all <- flood_valid; n <- nrow(x)
    if(!n) return(div(class="es-kpis",kpi("Recorded points","0",paste0("No points match the filters (",nrow(all)," valid in total)")),kpi("Unique coordinates","0",""),kpi("Latitude range","N/A",""),kpi("Longitude range","N/A",""),kpi("Valid geometries",paste0(nrow(all)," / ",flood_n_raw),"Whole dataset"),kpi("Missing descriptions","N/A","")))
    uniq <- n_distinct(x$Coord_key); miss <- sum(!x$Has_attr)
    div(class="es-kpis",
        kpi("Recorded points",fmt_n(n),if(n<nrow(all)) paste0("Filtered from ",fmt_n(nrow(all))," valid records") else "All valid records displayed"),
        kpi("Unique coordinates",fmt_n(uniq),if(n>uniq) paste0(n-uniq," record(s) repeat another record's coordinates") else "No repeated coordinate pairs"),
        kpi("Latitude range",paste0(round(min(x$Latitude),4),"–",round(max(x$Latitude),4)),paste0("°N · span ",round(diff(range(x$Latitude)),4),"°")),
        kpi("Longitude range",paste0(round(min(x$Longitude),4),"–",round(max(x$Longitude),4)),paste0("°E · span ",round(diff(range(x$Longitude)),4),"°")),
        kpi("Valid geometries",paste0(fmt_n(nrow(all))," / ",fmt_n(flood_n_raw)),paste0("Source features with usable WGS84 coordinates",if(flood_n_empty>0) paste0("; ",flood_n_empty," empty") else "")),
        kpi("Missing descriptions",paste0(fmt_n(miss)," (",round(100*miss/n,1),"%)"),"No source name and no description"))
  })
  
  # ---- Map ----
  output$flood_map <- renderLeaflet({
    validate(need(have_sf,"Install sf to read the KML: install.packages('sf')."),need(flood_ok,"KML not loaded or contains no mappable locations."))
    tile_opts <- providerTileOptions(errorTileUrl="data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7")
    leaflet(options=leafletOptions(minZoom=8,preferCanvas=TRUE)) |>
      addProviderTiles(providers$OpenStreetMap.Mapnik,group="OpenStreetMap",options=tile_opts) |>
      addProviderTiles(providers$CartoDB.Positron,group="Carto light",options=tile_opts) |>
      addLayersControl(baseGroups=c("OpenStreetMap","Carto light"),overlayGroups=c("Recorded points","Concentration grid","Selected point"),options=layersControlOptions(collapsed=TRUE)) |>
      hideGroup("Concentration grid") |>
      addScaleBar(position="bottomleft") |>
      fitBounds(min(flood_valid$Longitude),min(flood_valid$Latitude),max(flood_valid$Longitude),max(flood_valid$Latitude))
  })
  outputOptions(output,"flood_map",suspendWhenHidden=FALSE)
  
  # Markers, concentration grid and legend follow the filters and map options
  observe({
    x <- flood_f()
    proxy <- leafletProxy("flood_map") |> clearMarkerClusters() |> clearGroup(c("Recorded points","Concentration grid")) |> removeControl("flood_legend")
    if(!nrow(x)) return(invisible(NULL))
    grp <- NULL
    if(isTRUE(input$flood_color_group)) { pr <- flood_prox(); if(!is.null(pr) && !isTRUE(pr$skipped)) grp <- pr$group }
    pal <- c("#14866d","#3b7ea1","#a14b78","#e2a33b","#7a8f2a","#c0583c","#5b4fa3","#2a9d8f")
    fill <- if(is.null(grp)) rep("#ff9a76",nrow(x)) else ifelse(grp>0,pal[((pmax(grp,1)-1) %% length(pal))+1],"#9aa8a3")
    edge <- if(is.null(grp)) rep("#c4552d",nrow(x)) else ifelse(grp>0,"#183e37","#6c7a75")
    cs <- flood_cell(); g <- flood_grid(x,cs); gp <- colorNumeric(c("#d9efe8","#14866d","#0b3f35"),domain=c(0,max(g$n,2)))
    proxy <- proxy |> addRectangles(data=g,lng1=~lng1,lat1=~lat1,lng2=~lng2,lat2=~lat2,fillColor=~gp(n),fillOpacity=.45,color="#0b3f35",weight=.6,opacity=.5,group="Concentration grid",
                                    popup=~paste0(n," recorded point(s) in this ",cs," m grid cell<br><i>Concentration of recorded locations; not flood severity or probability.</i>"))
    proxy <- proxy |> addCircleMarkers(data=x,lng=~Longitude,lat=~Latitude,layerId=~Record,radius=7,color=edge,fillColor=fill,fillOpacity=.88,weight=1.4,
                                       label=lapply(htmlEscape(x$Label),HTML),popup=flood_popup(x,grp),group="Recorded points",
                                       clusterOptions=if(isTRUE(input$flood_cluster)) markerClusterOptions(showCoverageOnHover=FALSE,spiderfyOnMaxZoom=TRUE,zoomToBoundsOnClick=TRUE,maxClusterRadius=45,disableClusteringAtZoom=17) else NULL,
                                       clusterId=if(isTRUE(input$flood_cluster)) "flood_clusters" else NULL)
    if(is.null(grp)) proxy |> addLegend(layerId="flood_legend",position="bottomright",colors="#ff9a76",labels="Recorded location (KML point)",title="Recorded points",opacity=.9)
    else proxy |> addLegend(layerId="flood_legend",position="bottomright",colors=c("#14866d","#9aa8a3"),labels=c(paste0("In a proximity group (",flood_prox()$ngroups," groups; colour varies)"),paste0("Isolated (no neighbour within ",flood_eps()," m)")),title="Proximity (threshold)",opacity=.9)
  })
  # Highlight ring for the selected point
  observe({
    proxy <- leafletProxy("flood_map") |> clearGroup("Selected point")
    r <- flood_sel_row(); if(is.null(r)) return(invisible(NULL))
    proxy |> addCircleMarkers(lng=r$Longitude,lat=r$Latitude,radius=14,color="#0f4a3f",weight=3.5,fill=TRUE,fillColor="#ffffff",fillOpacity=.05,group="Selected point",options=pathOptions(interactive=FALSE))
  })
  fit_to <- function(lon,lat) {
    p <- leafletProxy("flood_map")
    if(!length(lon)) return(invisible(NULL))
    if(length(lon)==1 || (diff(range(lon))==0 && diff(range(lat))==0)) setView(p,lon[1],lat[1],zoom=16) else fitBounds(p,min(lon),min(lat),max(lon),max(lat))
  }
  observeEvent(input$flood_fit,{ x <- flood_f(); fit_to(x$Longitude,x$Latitude) })
  observeEvent(input$flood_reset,{
    updateSliderInput(session,"flood_id",value=flood_bounds$id); updateSliderInput(session,"flood_lat",value=flood_bounds$lat); updateSliderInput(session,"flood_lon",value=flood_bounds$lon)
    updateCheckboxInput(session,"flood_attr_only",value=FALSE); flood_sel(NULL)
    fit_to(flood_valid$Longitude,flood_valid$Latitude)
  })
  
  # ---- Right panel: selected location ----
  output$flood_selected <- renderUI({
    validate(need(flood_ok,"KML not loaded."))
    r <- flood_sel_row()
    if(is.null(r)) return(div(class="es-note",style="margin-top:0","No point selected. Click a marker on the map or a row in the records table to inspect it here."))
    x <- flood_f(); pr <- flood_prox(); ex <- flood_extra_text(r$Record)
    items <- list("Identifier"=if(r$Has_name) paste0(r$Label," (record ",r$Record,")") else paste0(r$Label),
                  "Latitude"=sprintf("%.6f °N",r$Latitude),"Longitude"=sprintf("%.6f °E",r$Longitude),
                  "Source name"=if(r$Has_name) r$Name else "Not provided in the source",
                  "Description"=if(r$Has_desc) substr(r$Description,1,400) else "Not provided in the source",
                  "Geometry"=r$Geometry_type %||% "Point")
    for(nm in names(ex)) items[[nm]] <- ex[[nm]]
    items[["Position"]] <- paste0(round(100*mean(x$Latitude<r$Latitude)),"% of displayed points lie south and ",round(100*mean(x$Longitude<r$Longitude)),"% lie west of it.")
    if(!is.null(pr) && !isTRUE(pr$skipped)) { i <- match(r$Record,pr$x$Record)
    if(!is.na(i)) { g <- pr$group[i]
    items[["Nearest point"]] <- paste0(fmt_n(round(pr$nn[i]))," m away",if(pr$nn[i]==0) " (identical coordinates)" else "")
    items[["Proximity group"]] <- if(g>0) paste0("Group ",g," with ",sum(pr$group==g)," points (record IDs: ",paste(head(pr$x$Record[pr$group==g],12),collapse=", "),if(sum(pr$group==g)>12) ", …" else "",") at ",pr$eps," m") else paste0("Isolated at ",pr$eps," m (no neighbour within the threshold)") } }
    tagList(kv_dl(items),
            if(!r$Has_attr) div(class="es-note","The source provides no descriptive attributes for this point; the label is a neutral identifier."),
            div(class="es-note","A KML coordinate is a recorded position, not a verified street address. No depth, severity or date is available."),
            actionButton("flood_clear","Clear selection",class="btn btn-outline-primary es-btn-sm"))
  })
  
  # ---- Right panel: geographic summary ----
  output$flood_geosummary <- renderUI({
    validate(need(flood_ok,"KML not loaded."))
    x <- flood_f(); if(!nrow(x)) return(div(class="es-note",style="margin-top:0","No displayed points."))
    la <- range(x$Latitude); lo <- range(x$Longitude); mid <- mean(la)
    h_km <- hav_dist(lo[1],la[1],lo[1],la[2])/1000; w_km <- hav_dist(lo[1],mid,lo[2],mid)/1000
    tagList(kv_dl(list("Points displayed"=paste0(fmt_n(nrow(x))," of ",fmt_n(nrow(flood_valid))," valid"),"Unique coordinates"=fmt_n(n_distinct(x$Coord_key)),
                       "Latitude extent"=paste0(sprintf("%.5f",la[1])," to ",sprintf("%.5f",la[2])," °N"),"Longitude extent"=paste0(sprintf("%.5f",lo[1])," to ",sprintf("%.5f",lo[2])," °E"),
                       "Bounding box"=paste0("SW (",sprintf("%.4f",la[1]),", ",sprintf("%.4f",lo[1]),") · NE (",sprintf("%.4f",la[2]),", ",sprintf("%.4f",lo[2]),")"),
                       "Box size"=paste0("≈ ",round(w_km,1)," km (E–W) × ",round(h_km,1)," km (N–S)"),
                       "Mean coordinate"=paste0(sprintf("%.4f",mean(x$Latitude)),", ",sprintf("%.4f",mean(x$Longitude))," (lat, lon)"))),
            div(class="es-note","The bounding box is the smallest rectangle containing the displayed points and the mean coordinate is a simple average. Neither is the flood-affected area or a named place."))
  })
  
  # ---- Right panel: dynamic observations ----
  output$flood_insights <- renderUI({
    validate(need(flood_ok,"KML not loaded."))
    x <- flood_f(); all <- flood_valid
    if(!nrow(x)) return(div(class="notice warning es-note",style="margin-top:0","No points match the current filters. Widen the ranges or press Reset filters."))
    n <- nrow(x); dups <- x |> count(Coord_key) |> filter(n>1); miss <- sum(!x$Has_attr)
    qla <- quantile(x$Latitude,c(.25,.5,.75),names=FALSE); qlo <- quantile(x$Longitude,c(.25,.5,.75),names=FALSE)
    g <- flood_grid(x,flood_cell()); top <- max(g$n)
    pr <- flood_prox(); r <- flood_sel_row()
    items <- list(
      insight_item("Displayed",paste0(fmt_n(n)," of ",fmt_n(nrow(all))," valid recorded points are shown",if(n<nrow(all)) " (filters active)." else ".")),
      insight_item("Coordinates",if(nrow(dups)) paste0(n_distinct(x$Coord_key)," unique coordinate pairs; ",nrow(dups)," pair(s) are shared by ",sum(dups$n)," records (identical to 6 decimals).") else paste0("All ",n," records have distinct coordinate pairs (6-decimal precision).")),
      insight_item("Metadata",paste0(miss," of ",n," records (",round(100*miss/n,1),"%) have neither a source name nor a description",if(miss==n) ". The source provides no descriptive text, so markers carry neutral identifiers." else paste0("; ",n-miss," carry source text."))),
      insight_item("Spread",paste0("The central half of the displayed points lies between ",sprintf("%.4f",qla[1])," and ",sprintf("%.4f",qla[3])," °N and ",sprintf("%.4f",qlo[1])," and ",sprintf("%.4f",qlo[3])," °E (median ",sprintf("%.4f",qla[2]),", ",sprintf("%.4f",qlo[2]),").")),
      insight_item("Concentration",paste0("In ",flood_cell()," m grid cells, ",nrow(g)," cells hold at least one point; the fullest cell holds ",top," point(s). This shows where records exist, not flood severity or probability.")))
    if(is.null(pr)) items <- c(items,list(insight_item("Proximity","At least two displayed points are needed for distance analyses.")))
    else if(isTRUE(pr$skipped)) items <- c(items,list(insight_item("Proximity","Distance analyses are skipped above 1,500 displayed points.")))
    else items <- c(items,list(insight_item("Proximity",paste0("Median nearest-neighbour distance ",fmt_n(round(median(pr$nn)))," m (range ",fmt_n(round(min(pr$nn)))," to ",fmt_n(round(max(pr$nn)))," m). At a ",pr$eps," m threshold there are ",pr$ngroups," proximity group(s) holding ",sum(pr$group>0)," points; ",sum(pr$group==0)," point(s) are isolated. This describes spacing of records only."))))
    if(!is.null(r)) items <- c(items,list(insight_item("Selection",paste0(r$Label," lies ",if(between(r$Latitude,min(all$Latitude),max(all$Latitude)) && between(r$Longitude,min(all$Longitude),max(all$Longitude))) "inside" else "outside"," the full dataset's coordinate extent (",sprintf("%.4f",min(all$Latitude)),"–",sprintf("%.4f",max(all$Latitude))," °N, ",sprintf("%.4f",min(all$Longitude)),"–",sprintf("%.4f",max(all$Longitude))," °E)."))))
    tagList(items)
  })
  
  # ---- Supporting spatial analytics ----
  output$flood_nn_plot <- renderPlotly({
    pr <- flood_prox()
    validate(need(!is.null(pr),"At least two displayed points are needed."),need(!isTRUE(pr$skipped),"Distance analyses are skipped above 1,500 displayed points."))
    h <- hist(pr$nn,breaks=25,plot=FALSE)
    shp <- list(list(type="line",x0=pr$eps,x1=pr$eps,y0=0,y1=1,yref="paper",line=list(color="#c0583c",dash="dot",width=1.5)))
    ann <- list(list(x=pr$eps,y=1,yref="paper",text=paste0("threshold ",pr$eps," m"),showarrow=FALSE,xanchor="left",font=list(size=10,color="#c0583c")))
    r <- flood_sel_row()
    if(!is.null(r)) { i <- match(r$Record,pr$x$Record)
    if(!is.na(i)) { shp[[2]] <- list(type="line",x0=pr$nn[i],x1=pr$nn[i],y0=0,y1=.9,yref="paper",line=list(color="#0f4a3f",dash="dash",width=1.5))
    ann[[2]] <- list(x=pr$nn[i],y=.9,yref="paper",text="selected point",showarrow=FALSE,xanchor="left",font=list(size=10,color="#0f4a3f")) } }
    lo <- h$breaks[-length(h$breaks)]; hi <- h$breaks[-1]
    plot_ly(x=h$mids,y=h$counts,type="bar",marker=list(color="#1a9679"),text=paste0(round(lo),"–",round(hi)," m<br>Points: ",h$counts),hoverinfo="text") |>
      base_layout(xaxis=list(title="Distance to nearest other displayed point (m)"),yaxis=list(title="Points",gridcolor="#e8f0ed"),shapes=shp,annotations=ann,bargap=0) |> cfg()
  })
  flood_groups <- reactive({
    pr <- flood_prox(); if(is.null(pr) || isTRUE(pr$skipped) || pr$ngroups==0) return(NULL)
    gs <- seq_len(pr$ngroups)
    mem <- lapply(gs,function(g) which(pr$group==g))
    tbl <- tibble(Group=gs,Points=lengths(mem),
                  `Record IDs`=vapply(mem,function(i) paste0(paste(head(pr$x$Record[i],10),collapse=", "),if(length(i)>10) ", …" else ""),character(1)),
                  `Centroid lat`=vapply(mem,function(i) mean(pr$x$Latitude[i]),numeric(1)),`Centroid lon`=vapply(mem,function(i) mean(pr$x$Longitude[i]),numeric(1)),
                  `Max pairwise distance (m)`=vapply(mem,function(i) max(pr$D[i,i],na.rm=TRUE),numeric(1)))
    list(tbl=tbl,mem=mem,x=pr$x)
  })
  output$flood_group_table <- renderDT({
    gr <- flood_groups(); validate(need(!is.null(gr),"No proximity groups at the current threshold (all displayed points are isolated, or fewer than two points are displayed)."))
    datatable(gr$tbl,rownames=FALSE,selection="single",options=list(pageLength=6,scrollX=TRUE,dom="tip")) |> formatRound(c("Centroid lat","Centroid lon"),5) |> formatRound("Max pairwise distance (m)",0)
  })
  output$flood_group_note <- renderUI({
    pr <- flood_prox(); if(is.null(pr) || isTRUE(pr$skipped)) return(NULL)
    div(class="es-note",paste0(pr$ngroups," group(s) of ≥2 points and ",sum(pr$group==0)," isolated point(s) at a ",pr$eps," m threshold. Isolated points play the role of 'noise' in DBSCAN terms. Use 'Colour by proximity group' above to see membership on the map."))
  })
  observeEvent(input$flood_group_table_rows_selected,{
    gr <- flood_groups(); i <- input$flood_group_table_rows_selected
    if(!is.null(gr) && length(i)==1 && i<=length(gr$mem)) { m <- gr$x[gr$mem[[i]],]; fit_to(m$Longitude,m$Latitude) }
  })
  
  # ---- Records table and download ----
  flood_table_data <- reactive({
    x <- flood_f() |> arrange(Record)
    pr <- flood_prox(); x$Proximity_group <- if(!is.null(pr) && !isTRUE(pr$skipped)) pr$group[match(x$Record,pr$x$Record)] else NA_integer_
    x
  })
  output$flood_table <- renderDT({
    validate(need(flood_ok,"No coordinates available."))
    x <- flood_table_data(); validate(need(nrow(x)>0,"No points match the current filters."))
    t <- x |> transmute(Record,Latitude,Longitude,`Source name`=Name,`Source description`=Description,`Proximity group (0 = isolated)`=Proximity_group)
    datatable(t,rownames=FALSE,filter="top",selection="single",options=list(pageLength=8,scrollX=TRUE)) |> formatRound(c("Latitude","Longitude"),6)
  })
  observeEvent(input$flood_table_rows_selected,{
    x <- flood_table_data(); i <- input$flood_table_rows_selected
    if(length(i)==1 && i<=nrow(x)) { flood_sel(x$Record[i]); leafletProxy("flood_map") |> setView(x$Longitude[i],x$Latitude[i],zoom=16) }
  })
  output$download_flood <- downloadHandler(filename=function()paste0("ecoshield_flood_coordinates_",Sys.Date(),".csv"),
                                           content=function(file) { x <- flood_table_data() |> select(Record,Latitude,Longitude,Name,Description,Geometry_type,Proximity_group)
                                           ex <- flood_extra_df; if(!is.null(ex)) x <- bind_cols(x,as_tibble(ex[x$Record,,drop=FALSE]))
                                           write_csv(x,file) })
  
  # ===================== AIR QUALITY PAGE OUTPUTS =====================
  output$aq_metrics <- renderUI({
    x <- aq_f(); has <- nrow(x)>0
    div(class="metric-grid aq-kpis",
        metric("Records in Selection",fmt_n(nrow(x)),"Valid numeric AQI observations"),
        metric("Mean Recorded AQI",if(has) aq_fmt(round(mean(x$AQI.Value),1)) else "N/A","Historical dataset statistic"),
        metric("Highest Recorded AQI",if(has) aq_fmt(max(x$AQI.Value)) else "N/A","Maximum in selection · not live"),
        metric("Monitoring Stations",fmt_n(n_distinct(x$Station)),"Stations in selection"))
  })
  
  output$aq_scale <- renderPlotly({
    x <- aq_f(); validate(need(nrow(x)>0,AQ_EMPTY))
    v <- x$AQI.Value; q <- quantile(v,c(.25,.5,.75),names=FALSE); m <- mean(v)
    cats <- x |> filter(!is.na(AQI.Cat)) |> group_by(AQI.Cat) |>
      summarise(Min=min(AQI.Value),Max=max(AQI.Value),Med=median(AQI.Value),N=n(),.groups="drop") |> arrange(desc(Med))
    k <- nrow(cats); ytop <- k+1; p <- plot_ly()
    for(i in seq_len(k)) {
      r <- cats[i,]; yy <- k-i+1
      p <- add_trace(p,x=c(r$Min,r$Max),y=c(yy,yy),type="scatter",mode="lines+markers",line=list(color="#a9d6c9",width=10),
                     marker=list(color="#5fa896",size=7),hoverinfo="text",showlegend=FALSE,
                     text=paste0("Supplied category: ",r$AQI.Cat,"<br>Observed AQI range in selection: ",aq_fmt(r$Min)," – ",aq_fmt(r$Max),"<br>Median: ",aq_fmt(round(r$Med,1)),"<br>Records: ",r$N))
    }
    p <- p |>
      add_trace(x=range(v),y=c(ytop,ytop),type="scatter",mode="lines",line=list(color="#cfe3dc",width=20),name="Lowest to highest",hoverinfo="text",
                text=paste0("Selected records<br>Lowest: ",aq_fmt(min(v)),"<br>Highest: ",aq_fmt(max(v)))) |>
      add_trace(x=c(q[1],q[3]),y=c(ytop,ytop),type="scatter",mode="lines",line=list(color="#14866d",width=20),name="Middle 50% (IQR)",hoverinfo="text",
                text=paste0("Middle 50% of records<br>",aq_fmt(round(q[1],1))," – ",aq_fmt(round(q[3],1)))) |>
      add_trace(x=q[2],y=ytop,type="scatter",mode="markers",marker=list(symbol="diamond",size=15,color="#e2a33b",line=list(color="#183e37",width=1)),
                name="Median",hoverinfo="text",text=paste0("Median: ",aq_fmt(round(q[2],1)))) |>
      add_trace(x=m,y=ytop,type="scatter",mode="markers",marker=list(symbol="circle",size=11,color="#183e37"),
                name="Mean",hoverinfo="text",text=paste0("Mean: ",aq_fmt(round(m,1))))
    pad <- max(1,(aq_rng[2]-aq_rng[1])*.03)
    p |> base_layout(xaxis=list(title="Recorded AQI value (numeric index, as supplied)",range=c(aq_rng[1]-pad,aq_rng[2]+pad),gridcolor="#e8f0ed",zeroline=FALSE),
                     yaxis=list(title="",tickvals=seq_len(k+1),ticktext=c(rev(as.character(cats$AQI.Cat)),"All selected records"),range=c(0.4,k+1.6),
                                zeroline=FALSE,showgrid=FALSE,fixedrange=TRUE,automargin=TRUE),
                     legend=list(orientation="h",y=-0.4,font=list(size=11))) |> cfg()
  })
  output$aq_scale_text <- renderUI({
    x <- aq_f()
    div(class="aq-explain",
        if(nrow(x)) p(style="margin:0 0 4px",tags$b("In this selection: "),"recorded values run from ",aq_fmt(min(x$AQI.Value))," to ",aq_fmt(max(x$AQI.Value)),", with a median of ",aq_fmt(round(median(x$AQI.Value),1)),"."),
        tags$ul(
          tags$li(tags$b("What the number is: "),"each record holds one AQI value, an index number that summarises measured pollutant levels at a station."),
          tags$li(tags$b("Higher or lower: "),"in AQI systems a higher number means more polluted air. This file does not name its AQI standard, so no category boundaries are drawn here. Indian (CPCB) and US AQI breakpoints differ and are never mixed."),
          tags$li(tags$b("Why this is not today's air quality: "),"the observation year is unverified and the records carry no verified date, so nothing here shows current conditions or health advice."),
          tags$li("Light bars are the observed range of each supplied category; text labels name every row.")))
  })
  
  output$aq_rank <- renderPlotly({
    x <- aq_f(); validate(need(nrow(x)>0,AQ_EMPTY))
    s <- aq_station_stats(x) |> arrange(Mean) |> mutate(lab=paste0(Station," (n=",N,")"),base=0)
    s$tip <- aq_tip(s); s$lab <- factor(s$lab,levels=s$lab)
    plot_ly(s) |>
      add_segments(x=~base,xend=~Mean,y=~lab,yend=~lab,line=list(color="#b9d3cb",width=3),hoverinfo="none",showlegend=FALSE) |>
      add_markers(x=~Mean,y=~lab,text=~tip,hoverinfo="text",marker=list(size=12,color="#14866d"),name="Mean") |>
      add_markers(x=~Median,y=~lab,text=~tip,hoverinfo="text",marker=list(size=9,symbol="diamond",color="#e2a33b",line=list(color="#183e37",width=1)),name="Median") |>
      base_layout(xaxis=list(title="Recorded AQI value",rangemode="tozero",gridcolor="#e8f0ed"),
                  yaxis=list(title="",automargin=TRUE,categoryorder="array",categoryarray=levels(s$lab)),legend=list(orientation="h",y=-0.12)) |> cfg()
  })
  
  output$aq_box <- renderPlotly({
    x <- aq_f(); validate(need(nrow(x)>0,AQ_EMPTY))
    s <- aq_station_stats(x) |> arrange(Median) |> mutate(lab=paste0(Station," (n=",N,")"))
    x <- x |> left_join(select(s,Station,lab),by="Station"); x$lab <- factor(x$lab,levels=s$lab)
    plot_ly(x,x=~AQI.Value,y=~lab,type="box",orientation="h",boxpoints="outliers",boxmean=TRUE,fillcolor="rgba(20,134,109,0.35)",
            line=list(color="#14866d"),marker=list(color="#c0583c",size=4),showlegend=FALSE) |>
      base_layout(xaxis=list(title="Recorded AQI value",gridcolor="#e8f0ed"),yaxis=list(title="",automargin=TRUE)) |> cfg()
  })
  
  output$aq_hist <- renderPlotly({
    x <- aq_f(); validate(need(nrow(x)>0,AQ_EMPTY)); v <- x$AQI.Value
    validate(need(length(v)>=2 && diff(range(v))>0,"At least two different recorded AQI values are needed to draw a distribution."))
    nb <- min(30,max(8,tryCatch(nclass.FD(v),error=function(e) 10)))
    br <- pretty(range(v),n=nb); h <- hist(v,breaks=br,plot=FALSE,right=FALSE,include.lowest=TRUE)
    bw <- diff(br)[1]; n <- length(v); lo <- h$breaks[-length(h$breaks)]; hi <- h$breaks[-1]
    tip <- paste0("Recorded AQI ",aq_fmt(lo)," to ",aq_fmt(hi),"<br>Observations: ",h$counts," (",sprintf("%.1f",h$counts/n*100),"% of selection)")
    d <- tryCatch(density(v,from=min(br),to=max(br)),error=function(e) NULL)
    ymax <- max(h$counts,if(!is.null(d)) d$y*n*bw,na.rm=TRUE)*1.08
    p <- plot_ly() |> add_bars(x=h$mids,y=h$counts,width=bw*0.96,text=tip,hoverinfo="text",marker=list(color="rgba(20,134,109,0.65)"),name="Observations")
    if(!is.null(d)) p <- add_lines(p,x=d$x,y=d$y*n*bw,line=list(color="#183e37",width=2),name="Density curve",hoverinfo="skip")
    p |> add_trace(x=rep(mean(v),2),y=c(0,ymax),type="scatter",mode="lines",line=list(color="#c0583c",dash="dash",width=2),name=paste0("Mean ",aq_fmt(round(mean(v),1))),hoverinfo="text",text=paste0("Mean: ",aq_fmt(round(mean(v),1)))) |>
      add_trace(x=rep(median(v),2),y=c(0,ymax),type="scatter",mode="lines",line=list(color="#e2a33b",dash="dot",width=2),name=paste0("Median ",aq_fmt(round(median(v),1))),hoverinfo="text",text=paste0("Median: ",aq_fmt(round(median(v),1)))) |>
      base_layout(xaxis=list(title="Recorded AQI value (numeric intervals)",gridcolor="#e8f0ed"),yaxis=list(title="Number of observations",gridcolor="#e8f0ed",range=c(0,ymax)),
                  bargap=0.02,legend=list(orientation="h",y=-0.2)) |> cfg()
  })
  
  output$aq_categories <- renderPlotly({
    x <- aq_f(); validate(need(nrow(x)>0,AQ_EMPTY))
    d <- x |> filter(!is.na(AQI.Cat)) |> group_by(AQI.Cat) |> summarise(N=n(),Min=min(AQI.Value),Max=max(AQI.Value),Med=median(AQI.Value),.groups="drop") |>
      mutate(Pct=N/sum(N)*100) |> arrange(N)
    validate(need(nrow(d)>0,"No valid supplied category labels in the current selection."))
    d$AQI.Cat <- factor(d$AQI.Cat,levels=d$AQI.Cat)
    plot_ly(d,x=~N,y=~AQI.Cat,type="bar",orientation="h",marker=list(color="#14866d"),text=~paste0(fmt_n(N)," (",sprintf("%.1f",Pct),"%)"),textposition="outside",cliponaxis=FALSE,
            hoverinfo="text",hovertext=~paste0("<b>",AQI.Cat,"</b><br>Records: ",N," (",sprintf("%.1f",Pct),"%)<br>Observed AQI range: ",aq_fmt(Min)," – ",aq_fmt(Max),"<br>Median AQI: ",aq_fmt(round(Med,1)))) |>
      base_layout(xaxis=list(title="Number of observations",range=c(0,max(d$N)*1.3),gridcolor="#e8f0ed"),yaxis=list(title="",automargin=TRUE)) |> cfg()
  })
  output$aq_cat_note <- renderUI({
    x <- aq_f(); if(!nrow(x)) return(NULL); bad <- sum(is.na(x$AQI.Cat))
    div(class="aq-explain","Boundaries for these categories are not documented in the file, so they are not treated as official Indian AQI classes; hover to see each category's observed AQI range. ",
        if(bad>0) paste0(bad," selected record(s) (",sprintf("%.1f",bad/nrow(x)*100),"%) have a missing or invalid label and are not counted here."))
  })
  
  output$aq_pollutants <- renderPlotly({
    x <- aq_f() |> filter(!is.na(AQI.Poll)); validate(need(nrow(x)>0,"No valid prominent-pollutant labels in the current selection."))
    d <- x |> count(AQI.Poll) |> mutate(Pct=n/sum(n)*100) |> arrange(n); d$AQI.Poll <- factor(d$AQI.Poll,levels=d$AQI.Poll)
    plot_ly(d,x=~n,y=~AQI.Poll,type="bar",orientation="h",marker=list(color="#3b7ea1"),text=~paste0(fmt_n(n)," (",sprintf("%.1f",Pct),"%)"),textposition="outside",cliponaxis=FALSE,
            hoverinfo="text",hovertext=~paste0("<b>",AQI.Poll,"</b><br>Records naming it prominent: ",n," (",sprintf("%.1f",Pct),"%)")) |>
      base_layout(xaxis=list(title="Number of records",range=c(0,max(d$n)*1.3),gridcolor="#e8f0ed"),yaxis=list(title="Prominent pollutant (label)",automargin=TRUE)) |> cfg()
  })
  output$aq_conc <- renderPlotly({
    x <- aq_f(); validate(need(nrow(x)>0,AQ_EMPTY))
    pl <- lapply(seq_len(nrow(aq_poll_cols)),function(i) {
      v <- x[[aq_poll_cols$name[i]]]; v <- v[is.finite(v)]; if(!length(v)) return(NULL)
      plot_ly(y=v,type="box",name=aq_poll_cols$label[i],boxpoints="outliers",fillcolor="rgba(59,126,161,0.35)",line=list(color="#3b7ea1"),marker=list(color="#c0583c",size=3)) |>
        layout(yaxis=list(title=aq_poll_cols$name[i]))
    })
    pl <- Filter(Negate(is.null),pl); validate(need(length(pl)>0,"No valid pollutant concentrations in the current selection."))
    subplot(pl,nrows=1,margin=0.05,titleY=TRUE) |> layout(showlegend=FALSE,paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)",font=list(size=11,color="#183e37")) |> cfg()
  })
  
  output$aq_bubble <- renderPlotly({
    x <- aq_f(); validate(need(nrow(x)>0,AQ_EMPTY))
    s <- aq_station_stats(x) |> filter(is.finite(SD)); validate(need(nrow(s)>0,"Variability needs at least two records per station in the current selection."))
    s$tip <- paste0(aq_tip(s),"<br>Spread (SD): ",aq_fmt(round(s$SD,1)))
    plot_ly(s,x=~Mean,y=~SD,type="scatter",mode="markers",text=~tip,hoverinfo="text",
            marker=list(size=~N,sizemode="area",sizeref=2*max(s$N)/(38^2),sizemin=7,color="rgba(20,134,109,0.6)",line=list(color="#14866d",width=1))) |>
      base_layout(xaxis=list(title="Mean recorded AQI",gridcolor="#e8f0ed"),yaxis=list(title="Spread of recorded AQI (standard deviation)",gridcolor="#e8f0ed",rangemode="tozero")) |> cfg()
  })
  output$aq_bubble_note <- renderUI({
    x <- aq_f(); if(!nrow(x)) return(NULL); s <- aq_station_stats(x); one <- sum(!is.finite(s$SD))
    div(class="aq-explain","Bubble area = number of observations. A station with a high mean but small spread has consistently high recorded values; a large spread means recorded values differ widely. ",
        if(one>0) paste0(one," station(s) with only one record are not shown because spread cannot be calculated."))
  })
  
  if(nrow(aq_geo)>0) output$aq_map <- renderLeaflet({
    x <- aq_f(); validate(need(nrow(x)>0,AQ_EMPTY)); s <- aq_station_stats(x) |> inner_join(aq_geo,by="Station"); validate(need(nrow(s)>0,"No station coordinates for this selection."))
    leaflet(s) |> addTiles() |> addCircleMarkers(lng=~Longitude,lat=~Latitude,radius=~pmax(5,pmin(18,4+sqrt(N))),color="#14866d",fillOpacity=.6,
                                                 popup=~paste0("<b>",htmltools::htmlEscape(Station),"</b><br>Mean recorded AQI: ",aq_fmt(round(Mean,1)),"<br>Records: ",N))
  })
  
  output$aq_insight <- renderUI({
    x <- aq_f(); if(!nrow(x)) return(div(class="notice info",AQ_EMPTY))
    s <- aq_station_stats(x); k <- nrow(s); v <- x$AQI.Value
    top <- slice_max(s,Mean,n=1,with_ties=FALSE); low <- slice_min(s,Mean,n=1,with_ties=FALSE)
    el <- filter(s,N>=AQ_MIN_N,is.finite(SD)); vr <- if(nrow(el)) slice_max(el,SD,n=1,with_ties=FALSE) else NULL
    cat_d <- x |> filter(!is.na(AQI.Cat)) |> count(AQI.Cat,sort=TRUE); poll_d <- x |> filter(!is.na(AQI.Poll)) |> count(AQI.Poll,sort=TRUE)
    pct <- function(a,b) sprintf("%.1f%%",a/b*100)
    tagList(
      insight_item("Records",fmt_n(nrow(x))," valid observation(s) from ",k," station(s)."),
      insight_item("Highest mean",if(k>1) paste0(top$Station," (mean ",aq_fmt(round(top$Mean,1)),", ",top$N," records).") else paste0("Only one station in this selection: ",top$Station,".")),
      insight_item("Lowest mean",if(k>1) paste0(low$Station," (mean ",aq_fmt(round(low$Mean,1)),", ",low$N," records).") else "Not applicable with a single station."),
      insight_item("Variability",if(!is.null(vr)) paste0(vr$Station," has the widest spread (SD ",aq_fmt(round(vr$SD,1)),", range ",aq_fmt(vr$Min)," – ",aq_fmt(vr$Max),"; stations with at least ",AQ_MIN_N," records compared).") else paste0("Not reported: no station has at least ",AQ_MIN_N," records.")),
      insight_item("Median",paste0(aq_fmt(round(median(v),1))," (middle 50% of values: ",aq_fmt(round(quantile(v,.25,names=FALSE),1))," – ",aq_fmt(round(quantile(v,.75,names=FALSE),1)),").")),
      insight_item("Category",if(nrow(cat_d)) paste0("Most frequent supplied label: ",cat_d$AQI.Cat[1]," (",pct(cat_d$n[1],sum(cat_d$n)),"of labelled records).") else "No valid category labels."),
      insight_item("Pollutant",if(nrow(poll_d)) paste0("Most frequently named prominent pollutant: ",poll_d$AQI.Poll[1]," (",pct(poll_d$n[1],sum(poll_d$n))," of labelled records).") else "No valid pollutant labels."),
      insight_item("Missing",paste0("Without a valid category label: ",pct(sum(is.na(x$AQI.Cat)),nrow(x)),"; without a valid pollutant label: ",pct(sum(is.na(x$AQI.Poll)),nrow(x)),". Across the whole file, ",pct(aq_dq$aqi_missing+aq_dq$aqi_nonnumeric,max(aq_dq$kept_rows,1))," of AQI values were missing or non-numeric.")),
      div(class="aq-explain",if(k>1) paste0("Among the currently selected records, ",top$Station," has the highest mean recorded AQI. This describes the supplied observations only and does not establish that the station currently has the poorest air quality.") else "These figures describe the supplied observations only and say nothing about current air quality.",
          if(k>1 && top$N<AQ_MIN_N) paste0(" Note that ",top$Station," has only ",top$N," record(s), so its mean is based on very little data.")))
  })
  
  output$aq_quality <- renderUI({
    if(!nrow(aq_x)) return(p("The air-quality file was not found or lacks Station / AQI.Value columns."))
    d <- aq_dq; ok <- sum(is.finite(aq_x$AQI.Value))
    tagList(
      tags$h4("Records"),
      tags$ul(tags$li(fmt_n(d$raw_rows)," rows in the file; ",fmt_n(d$no_station)," without a station name were excluded; ",fmt_n(ok)," rows have a usable numeric AQI value."),
              tags$li(fmt_n(d$aqi_missing)," AQI values were blank and ",fmt_n(d$aqi_nonnumeric)," were non-numeric text. They are left out of statistics and never filled in."),
              tags$li(fmt_n(d$aqi_negative)," value(s) below 0 and ",fmt_n(d$aqi_over500)," above 500 were found; they are kept and flagged only, because the AQI scale is not documented in the file."),
              tags$li(fmt_n(d$dup_rows)," fully identical row(s) exist. They are kept: without verified dates or times they cannot be told apart from genuine repeat measurements."),
              if(d$station_variants>0) tags$li(d$station_variants," station name(s) differ from another only by case or punctuation; they are not merged automatically.")),
      tags$h4("Labels and columns"),
      tags$ul(tags$li("Category labels found: ",if(length(d$cat_labels)) paste(d$cat_labels,collapse=", ") else "none"," (",fmt_n(d$bad_cat)," records have a blank or placeholder label such as '-' or 'N/A (PC Issue)', which is treated as missing). Labels are only whitespace-trimmed."),
              tags$li("Prominent-pollutant labels found: ",if(length(d$poll_labels)) paste(d$poll_labels,collapse=", ") else "none"," (",fmt_n(d$bad_poll)," blank or placeholder)."),
              tags$li(if(nrow(aq_poll_cols)) paste0("Numeric pollutant columns used: ",paste(aq_poll_cols$name,collapse="; "),if(any(aq_poll_cols$unit=="unit not stated in source")) ". Where no unit is written in the column name, the unit is not stated in the source." else ".",if(d$poll_neg>0) paste0(" ",d$poll_neg," negative concentration value(s) were treated as missing.")) else "No numeric pollutant-concentration columns were found."),
              tags$li(if(length(d$date_cols)) paste0("Column(s) that look date-related: ",paste(d$date_cols,collapse=", "),". They have not been verified, so no time series is drawn.") else "The file has no date or year column, so no time series is possible and the observation year is unknown."),
              tags$li(if(d$geo_cols) "Latitude/longitude columns were found and used for the map." else "No station coordinates in the file, so no map is shown.")),
      tags$h4("Methods"),
      tags$ul(tags$li("Statistics are plain means, medians, quartiles and standard deviations of the supplied AQI values; the box-plot whiskers follow the standard 1.5 × IQR rule."),
              tags$li("No AQI standard is named in the file, so no CPCB (Indian) or US EPA thresholds are applied and no health categories are invented. Add the documented standard to the methodology notes if the source confirms it.")))
  })
  
  output$aq_table <- renderDT(datatable(select(aq_f(),-AQI.Cat,-AQI.Poll),rownames=FALSE,filter="top",options=list(pageLength=10,scrollX=TRUE)))
  output$download_aq <- downloadHandler(filename=function()paste0("ecoshield_air_quality_",Sys.Date(),".csv"),content=function(file)write_csv(select(aq_f(),-AQI.Cat,-AQI.Poll),file))
  
  output$park_metrics <- renderUI({x<-parks_f();div(class="metric-grid",metric("Listed records",format(nrow(x),big.mark=","),"After current filters"),metric("Distinct names",n_distinct(x$Park),"Not area measurements"),metric("Zones represented",n_distinct(x$Zone),"Current selection"),metric("Wards represented",n_distinct(x$Ward),"Current selection"))})
  output$park_zone_plot <- renderPlotly({x<-parks_f()|>count(Zone,name="ParkCount",sort=TRUE);validate(need(nrow(x)>0,"No park records match."));p<-ggplot(x,aes(ParkCount,reorder(Zone,ParkCount),text=paste0("Zone: ",Zone,"<br>Listed records: ",ParkCount)))+geom_col(fill="#14866d")+labs(x="Number of listed parks",y="Zone")+theme_minimal();ggplotly(p,tooltip="text")|>config(displaylogo=FALSE,responsive=TRUE)})
  output$park_ward_plot <- renderPlotly({x<-parks_f()|>count(Ward,name="ParkCount")|>arrange(ParkCount,Ward)|>slice_head(n=15);validate(need(nrow(x)>0,"No park records match."));p<-ggplot(x,aes(ParkCount,reorder(Ward,ParkCount),text=paste0("Ward: ",Ward,"<br>Listed records: ",ParkCount)))+geom_col(fill="#d8894c")+labs(x="Number of listed parks",y="Ward")+theme_minimal();ggplotly(p,tooltip="text")|>config(displaylogo=FALSE,responsive=TRUE)})
  output$park_bubble <- renderPlotly({x<-parks_f()|>group_by(Zone)|>summarise(Listed=n(),Wards=n_distinct(Ward),.groups="drop");validate(need(nrow(x)>0,"No zone records match the filters."));p<-ggplot(x,aes(Listed,Wards,size=Listed,text=paste0("Zone: ",Zone,"<br>Listed records: ",Listed,"<br>Wards represented: ",Wards)))+geom_point(alpha=.7,color="#14866d")+labs(x="Listed park records",y="Wards represented",size="Listed records")+theme_minimal();ggplotly(p,tooltip="text")|>config(displaylogo=FALSE,responsive=TRUE)})
  output$park_insight <- renderUI({x<-parks_f();if(!nrow(x))return(p("No park records match the selected filters."));z<-x|>count(Zone,sort=TRUE);w<-x|>count(Ward,sort=TRUE);div(class="notice info",tags$p(tags$b("Listed records in view: "),nrow(x)),tags$p(tags$b("Zone with most listed records in view: "),z$Zone[1]," (",z$n[1],")."),tags$p(tags$b("Ward with most listed records in view: "),w$Ward[1]," (",w$n[1],")."),tags$p("Interpretation: these are inventory-record counts only. They do not measure park area, quality, access, or green cover."))})
  
  output$park_table <- renderDT(datatable(parks_f(),rownames=FALSE,filter="top",options=list(pageLength=10,scrollX=TRUE)))
  output$download_parks <- downloadHandler(filename=function()paste0("ecoshield_parks_",Sys.Date(),".csv"),content=function(file)write_csv(parks_f(),file))
  
  # =================== LIVE ENVIRONMENTAL MONITOR (server) ===================
  lv_state <- reactiveVal(live_snapshot())
  lv_sel <- reactiveVal(NULL)
  lv_prev_max <- reactiveVal(NULL)
  lv_clock <- reactiveTimer(60*1000)                                   # only re-evaluates statuses/ages; makes NO network request
  lv_tick <- reactiveTimer(live_cfg$refresh_min*60*1000)               # retrieval timer (>= 10 min)
  lv_do <- function(force=FALSE) {
    if(!identical(input$nav,"Action Plan")) return(invisible())         # poll only while this tab is open
    ran <- tryCatch(isTRUE(live_refresh(force)),error=function(e) { message("Live refresh error: ",conditionMessage(e)); FALSE })
    lv_state(live_snapshot())
    invisible(ran)
  }
  observeEvent(lv_tick(), lv_do(FALSE), ignoreInit=TRUE)
  observeEvent(input$nav, lv_do(FALSE))
  observeEvent(input$lv_refresh, { if(!isTRUE(lv_do(TRUE))) showNotification(paste0("A retrieval was made less than ",round(live_cfg$min_gap_sec/60)," min ago; the sources are not queried again so soon."),type="message",duration=6) })
  
  lv_valid <- reactive({ d <- lv_state()$rain$data; if(is.null(d)) tibble() else filter(d,Valid) })
  lv_rain_st <- reactive({ lv_clock(); lv_status(lv_state()$rain,"daily") })
  lv_chip <- function(st) span(class=paste("lv-chip",st$cls),st$label)
  
  observeEvent(lv_state(), {
    d <- lv_state()$rain$data
    if(!is.null(d)) updateSelectInput(session,"lv_net",choices=c("All networks",sort(unique(d$Network))),selected=isolate(input$lv_net) %||% "All networks")
    a <- lv_state()$archive
    if(nrow(a)) {
      dts <- as.Date(a$Obs_end,tz=LIVE_TZ)
      updateSelectizeInput(session,"lv_ts_station",choices=sort(unique(a$Station)),selected=isolate(input$lv_ts_station))
      cur <- isolate(input$lv_dates); prev_max <- isolate(lv_prev_max())
      st <- if(is.null(prev_max) || is.null(cur) || cur[1] < min(dts) || cur[1] > max(dts)) min(dts) else cur[1]
      en <- if(is.null(prev_max) || is.null(cur) || cur[2] >= prev_max || cur[2] > max(dts)) max(dts) else cur[2]   # follow new days only if the user was at the latest day
      updateDateRangeInput(session,"lv_dates",min=min(dts),max=max(dts),start=st,end=max(st,en))
      lv_prev_max(max(dts))
    }
  })
  
  output$lv_banner <- renderUI({
    lv_clock(); s <- lv_state(); out <- list()
    if(live_cfg$tn_enabled && is.null(s$rain$data) && is.na(s$rain$error)) out <- c(out,list(notice("Loading rainfall data.",
                                                                                                    " Retrieving the latest TN-SMART daily report. Nothing is shown until real station readings have been read and validated.","info")))
    if(live_cfg$tn_enabled && !is.na(s$rain$error)) out <- c(out,list(
      if(is.null(s$rain$data)) notice("Rainfall data unavailable.",paste0(" The latest retrieval from TN-SMART failed (",s$rain$error,"). No values are shown rather than estimated ones. Official page: ",LV_LINKS$tn),"warning")
      else notice("STALE DATA: the latest refresh failed.",paste0(" Showing the last valid report (source date ",lv_fmt_d(s$rain$meta$rep_date),"; retrieved ",lv_fmt_t(s$rain$last_ok),"). It may be out of date and the app will retry automatically. Reason: ",s$rain$error),"warning")))
    if(live_cfg$tn_enabled && is.na(s$rain$error) && !is.na(s$rain$recovered_at) && as.numeric(difftime(Sys.time(),s$rain$recovered_at,units="mins")) < 60)
      out <- c(out,list(notice("Recovered.",paste0(" TN-SMART data was retrieved successfully at ",lv_fmt_t(s$rain$recovered_at)," after an earlier failed refresh."),"info")))
    if(!live_cfg$imd_enabled) out <- c(out,list(notice("IMD weather and warnings are not enabled.",
                                                       " IMD's public API only answers servers whose public IP IMD has whitelisted (free for non-commercial use). After IMD approves your IP, set ECOSHIELD_IMD_ENABLED=true. Until then, see the official pages linked below. Missing warning data does not mean there are no warnings.","info")))
    else if(!is.na(s$wx$error) || !is.na(s$warn$error)) out <- c(out,list(notice("IMD retrieval problem.",paste0(" ",paste(na.omit(c(if(!is.na(s$wx$error)) paste0("Weather: ",s$wx$error),if(!is.na(s$warn$error)) paste0("Warnings: ",s$warn$error))),collapse=" | ")),"warning")))
    if(!live_cfg$tn_enabled) out <- c(out,list(notice("TN-SMART retrieval is switched off."," Set ECOSHIELD_TNSMART_ENABLED=true to retrieve Chennai rainfall.","info")))
    tagList(out)
  })
  output$lv_refresh_info <- renderUI({
    lv_clock(); s <- lv_state()
    m <- s$rain$meta; stale <- live_cfg$tn_enabled && !is.null(s$rain$data) && !is.na(s$rain$error)
    div(class="lv-small",if(stale) span(class="lv-chip warn","STALE · last refresh failed"),if(stale) " ",tags$b("Last successful retrieval: "),lv_fmt_t(s$rain$last_ok)," · ",tags$b("Source report date: "),lv_fmt_d(m$rep_date)," · ",tags$b("Observation period: "),lv_period_label(m)," · automatic check every ",live_cfg$refresh_min," min (the daily report itself changes far less often) · refreshing this page does not make data live.")
  })
  
  # ---- KPI cards ----
  output$lv_kpis <- renderUI({
    lv_clock(); s <- lv_state(); v <- lv_valid(); m <- s$rain$meta
    k1 <- if(nrow(v)) { top <- v[which.max(v$Rainfall_mm),]; ties <- sum(v$Rainfall_mm==top$Rainfall_mm); stale <- !is.na(s$rain$error)
    kpi("Highest reported station rainfall",lv_mmcm(top$Rainfall_mm),
        tagList(div(paste0("Station: ",top$Station,if(ties>1) paste0(" (+",ties-1," other station(s) with the same reading)") else "")),
                div(paste0("Observation period: ",lv_period_label(m))),div(paste0("Source report date: ",lv_fmt_d(m$rep_date))),
                div(paste0("Retrieved: ",lv_fmt_t(s$rain$last_ok),if(stale) " · STALE (latest refresh failed)" else "")),
                div("A single station reading. Not a Chennai-wide total or average."))) }
    else kpi("Highest reported station rainfall","Unavailable","No valid TN-SMART station readings retrieved")
    k2 <- if(!is.null(s$rain$data)) kpi("Valid station records",paste0(nrow(v)," of ",nrow(s$rain$data)),paste0(nrow(s$rain$data)-nrow(v)," rejected (missing, malformed or conflicting) · ",if(length(s$rain$audit)) s$rain$audit$zeros else 0," valid 0 mm reading(s) kept · source: TN-SMART"))
    else kpi("Valid station records","Unavailable","No station list retrieved")
    wx <- s$wx$data; w1 <- if(!is.null(wx)) wx |> filter(Temp_ok) |> slice_head(n=1) else NULL
    k3 <- if(!is.null(w1) && nrow(w1)) kpi("Latest temperature · humidity",paste0(formatC(w1$Temp,format="f",digits=1)," °C · ",if(w1$RH_ok) paste0(round(w1$RH)," %") else "humidity unavailable"),
                                           paste0("IMD observation, ",w1$Station,", ",lv_fmt_t(with_tz(w1$Obs_utc,LIVE_TZ))))
    else kpi("Latest temperature · humidity","Unavailable",if(live_cfg$imd_enabled) "No valid IMD observation retrieved" else "IMD API not enabled (needs whitelisted IP)")
    wd <- s$warn$data
    k4 <- if(!is.null(wd) && nrow(wd)) { d1 <- wd[wd$Day==1,]; mx <- wd[which.max(ifelse(is.na(wd$Color),0,wd$Color)),]
    kpi("Current official weather alerts",if(is.na(d1$Color)) "Unavailable" else sub(" ·.*","",d1$Level),
        tagList(div(paste0("Day 1: ",d1$Description)),div(paste0("Highest in 5-day outlook: ",sub(" ·.*","",mx$Level)," (Day ",mx$Day,")")),div("Source: IMD district warning (forecast)"))) }
    else kpi("Current official weather alerts","Unavailable",if(live_cfg$imd_enabled) "No warning data retrieved; this is not confirmation of no warnings" else "IMD API not enabled; this is not confirmation of no warnings")
    div(class="lv-kpis",k1,k2,k3,k4)
  })
  
  # ---- Status panel ----
  output$lv_status <- renderUI({
    lv_clock(); s <- lv_state(); st <- lv_rain_st()
    wst <- lv_status(s$wx,"obs"); wr <- lv_status(s$warn,"forecast")
    row <- function(name,url,type,obs,ok,chip,recs,bad) tags$tr(tags$td(tags$a(href=url,target="_blank",name)),tags$td(type),tags$td(obs),tags$td(ok),tags$td(chip),tags$td(recs),tags$td(bad))
    rn <- s$rain$audit
    div(class="lv-tbl-wrap",tags$table(class="lv-tbl",
                                       tags$tr(lapply(c("Source","Type","Observation timestamp","Last successful refresh","Status","Reporting","Missing / invalid"),tags$th)),
                                       row("TN-SMART Chennai",LV_LINKS$tn,"Observation: 24-h accumulation",lv_fmt_t(s$rain$meta$obs_time),lv_fmt_t(s$rain$last_ok),lv_chip(st),
                                           if(length(rn)) paste0(rn$valid," stations") else "Unavailable", if(length(rn)) paste0(rn$invalid," station reading(s)") else "Unavailable"),
                                       row("IMD current weather",LV_LINKS$imd_ref,"Observation",lv_fmt_t(s$wx$meta$obs_time),lv_fmt_t(s$wx$last_ok),lv_chip(wst),
                                           if(length(s$wx$audit)) paste0(s$wx$audit$rows," record(s)") else "Unavailable", if(length(s$wx$audit)) paste0(s$wx$audit$temp_invalid+s$wx$audit$rh_invalid," value(s); ",s$wx$audit$ts_invalid," bad timestamp(s)") else "Unavailable"),
                                       row("IMD district warning",LV_LINKS$imd_ref,"Forecast warning (Day 1 to 5)",paste0("Issued ",lv_fmt_t(s$warn$meta$obs_time)),lv_fmt_t(s$warn$last_ok),
                                           if(wr$cls=="ok") span(class="lv-chip fc",wr$label) else lv_chip(wr), if(length(s$warn$audit)) paste0(s$warn$audit$rows," day entries") else "Unavailable", if(length(s$warn$audit)) paste0(s$warn$audit$bad_color," colour code(s)") else "Unavailable"),
                                       row("IMD Chennai centre",LV_LINKS$imd_chennai,"Portal (link only)","n/a","n/a",span(class="lv-chip off","No programmatic access verified"),"n/a","n/a")),
        if(length(rn) && is.finite(s$rain$meta$src_avg)) div(class="lv-note",paste0("Parser integrity check (not a Chennai-wide figure): the source states an average of ",formatC(s$rain$meta$src_avg,format="f",digits=2)," mm; recomputed from the valid rows: ",formatC(rn$calc_avg,format="f",digits=2)," mm (",if(isTRUE(rn$avg_match)) "match" else "MISMATCH, treat with caution",")."))
        else NULL)
  })
  
  # ---- Insights ----
  output$lv_insights <- renderUI({
    lv_clock(); s <- lv_state(); v <- lv_valid(); m <- s$rain$meta; items <- list()
    if(nrow(v)) {
      per <- lv_period_label(m); top <- v[which.max(v$Rainfall_mm),]; n_all <- nrow(s$rain$data)
      cls <- table(v$Class); cls <- cls[cls>0]
      rk <- head(arrange(v,desc(Rainfall_mm)),5)
      nb <- v |> group_by(Network) |> slice_max(Rainfall_mm,n=1,with_ties=FALSE) |> ungroup()
      n_heavy <- sum(as.integer(v$Class)>=5,na.rm=TRUE)
      items <- c(items,list(
        insight_item("Highest",paste0(top$Station," reported ",lv_mmcm(top$Rainfall_mm)," (",per,").")),
        insight_item("Top 5",paste0(paste0(seq_len(nrow(rk)),". ",rk$Station," ",lv_mm(rk$Rainfall_mm),collapse="; "),".")),
        if(nrow(nb)>1) insight_item("By network",paste0("Highest reading in each reporting network: ",paste0(nb$Network,": ",nb$Station," ",lv_mm(nb$Rainfall_mm),collapse="; "),".")),
        insight_item("Heavy rain",paste0(n_heavy," of ",nrow(v)," valid station(s) reported more than 64.4 mm (heavy or above on the TN-SMART legend) in this period. Single-station readings are not an area-wide measurement.")),
        insight_item("Stations",paste0(nrow(v)," of ",n_all," listed station records are valid; ",n_all-nrow(v)," rejected. ",sum(v$Rainfall_mm==0)," valid station(s) reported 0 mm (an observation, not missing data).")),
        insight_item("Classes",paste0("Station readings by rainfall class (TN-SMART legend): ",paste0(names(cls)," ",as.integer(cls),collapse="; "),". These classify single station totals only; they are not official warnings.")),
        insight_item("Timestamp",paste0("Source report date ",lv_fmt_d(m$rep_date),"; observation period: ",per,". Data retrieved ",lv_fmt_t(s$rain$last_ok),if(!is.na(s$rain$error)) " (STALE: latest refresh failed)" else "",".")))) 
      items <- Filter(Negate(is.null),items)
    } else items <- c(items,list(insight_item("Rainfall","No valid station readings are available, so no rainfall insights can be generated.")))
    wd <- s$warn$data
    items <- c(items,list(if(!is.null(wd) && nrow(wd)) { act <- wd[!is.na(wd$Color) & wd$Color>=2,]
    insight_item("Warnings",if(nrow(act)) paste0("IMD lists ",paste0("Day ",act$Day,": ",sub(" ·.*","",act$Level)," (",act$Description,")",collapse="; ")," for ",wd$District[1],".") else paste0("IMD's latest district warning for ",wd$District[1]," lists no warning for Days 1 to 5.")) }
    else insight_item("Warnings","No official warning data retrieved; this does not mean that no warnings are in force."),
    div(class="lv-note","Rainfall observations alone cannot show that any area is flooded, and nothing here is a prediction.")))
    tagList(items)
  })
  
  # ---- Map (only with verified coordinates) ----
  lv_map_data <- reactive({ v <- lv_valid(); if(!nrow(v) || !nrow(lv_coords)) return(tibble()); inner_join(mutate(v,Key=lv_key(Station)),lv_coords,by="Key") })
  output$lv_map_ui <- renderUI({
    md <- lv_map_data(); v <- lv_valid()
    if(nrow(md)) tagList(leafletOutput("lv_map",height="520px"),div(class="lv-note",paste0(nrow(md)," of ",nrow(v)," stations have verified coordinates and are plotted; the rest appear in the chart and table only. No rainfall is interpolated between stations.")))
    else div(class="lv-note",tags$b("Map not drawn: "),"the TN-SMART report lists station names with zone and ward numbers but no coordinates, and coordinates are never guessed. Add a CSV of verified coordinates (columns Station, Latitude, Longitude, Coordinate_Source; Station must match the name exactly) and set ECOSHIELD_STATION_COORDS to its path. Use 'Download coordinate template' below. The ranked chart and table below show every station.")
  })
  output$lv_map <- renderLeaflet({
    md <- lv_map_data(); req(nrow(md)>0); m <- lv_state()$rain$meta; st <- lv_rain_st()
    md$popup <- paste0("<b>",htmltools::htmlEscape(md$Station),"</b><br>Rainfall: ",lv_mm(md$Rainfall_mm)," (",lv_cm(md$Rainfall_cm*10),")<br>Unit: millimetres (cm = mm / 10)<br>Period: ",htmltools::htmlEscape(md$Period),
                       "<br>Observation time: ",lv_fmt_t(m$obs_time),"<br>Source status: ",htmltools::htmlEscape(st$label),"<br>Coordinates: ",htmltools::htmlEscape(ifelse(is.na(md$Coord_source),"supplied by user",md$Coord_source)))
    leaflet(md) |> addTiles() |>
      addCircleMarkers(lng=~Longitude,lat=~Latitude,layerId=~Label,radius=9,color="#183e37",weight=1,fillColor=LV_CLASS_COL[as.integer(md$Class)],fillOpacity=.85,popup=~popup) |>
      addLegend("bottomright",colors=LV_CLASS_COL,labels=paste0(LV_CLASSES," (",LV_CLASS_RANGES,")"),title="24-h rainfall class (TN-SMART legend)",opacity=.9)
  })
  observeEvent(input$lv_map_marker_click, lv_sel(input$lv_map_marker_click$id))
  
  # ---- Ranked chart ----
  lv_rank_data <- reactive({
    v <- lv_valid(); if(!nrow(v)) return(v)
    if(!is.null(input$lv_net) && input$lv_net!="All networks") v <- filter(v,Network==input$lv_net)
    v <- arrange(v,desc(Rainfall_mm)); n <- suppressWarnings(as.integer(input$lv_top %||% "20"))
    if(is.finite(n) && n>0) v <- head(v,n); v
  })
  output$lv_rank_ui <- renderUI({ n <- nrow(lv_rank_data()); plotlyOutput("lv_rank",height=paste0(max(300,n*17+90),"px")) })
  output$lv_rank <- renderPlotly({
    d <- lv_rank_data(); validate(need(nrow(d)>0,"No valid station readings for the current selection (missing values are not treated as zero)."))
    d <- arrange(d,Rainfall_mm); lab <- as.character(d$Label); d$Label <- factor(d$Label,levels=d$Label)
    d$tip <- paste0("<b>",htmltools::htmlEscape(d$Station),"</b><br>Rainfall: ",lv_mm(d$Rainfall_mm)," = ",lv_cm(d$Rainfall_mm),"<br>Class: ",d$Class,"<br>Network: ",d$Network,
                    "<br>Period: ",htmltools::htmlEscape(d$Period),"<br>Observation time: ",lv_fmt_t(d$Obs_end[1]),"<br>Source: ",d$Source)
    plot_ly(d,x=~Rainfall_mm,y=~Label,type="bar",orientation="h",text=~tip,hoverinfo="text",customdata=lab,source="lv_rank",marker=list(color=LV_CLASS_COL[as.integer(d$Class)],line=list(color="#183e37",width=.5))) |>
      base_layout(xaxis=list(title="Reported rainfall, 24 h to 08:30 IST (mm)",gridcolor="#e8f0ed",rangemode="tozero"),yaxis=list(title="",automargin=TRUE,tickfont=list(size=10),categoryorder="array",categoryarray=levels(d$Label))) |> cfg() |> event_register("plotly_click")
  })
  observeEvent(event_data("plotly_click",source="lv_rank"), { e <- event_data("plotly_click",source="lv_rank"); if(!is.null(e$customdata)) lv_sel(as.character(e$customdata[1])) })
  
  # ---- Time series (retrieved daily reports only) ----
  lv_ts_data <- reactive({
    a <- lv_state()$archive; if(!nrow(a)) return(a)
    a$Date <- as.Date(a$Obs_end,tz=LIVE_TZ)
    if(!is.null(input$lv_dates)) a <- filter(a,Date>=input$lv_dates[1],Date<=input$lv_dates[2]); a
  })
  output$lv_ts <- renderPlotly({
    a <- lv_ts_data(); validate(need(nrow(lv_state()$archive)>0,"No daily report has been retrieved yet."),need(nrow(a)>0,"No retrieved reports in this date range."))
    validate(need(n_distinct(a$Obs_end)>=2,"Only one daily report has been retrieved so far. A time series needs at least two; earlier days are not back-filled or invented."))
    sel <- input$lv_ts_station
    if(length(sel)) { d <- filter(a,Station %in% sel)
    validate(need(nrow(d)>0,"The selected stations have no retrieved reports in this date range."))
    d$tip <- paste0("<b>",htmltools::htmlEscape(d$Station),"</b><br>Observation (period end): ",format(d$Obs_end,"%d %b %Y %H:%M",tz=LIVE_TZ)," IST<br>Rainfall: ",lv_mm(d$Rainfall_mm)," = ",lv_cm(d$Rainfall_mm),"<br>Period: ",htmltools::htmlEscape(d$Period),"<br>Source: TN-SMART daily report")
    p <- plot_ly(d,x=~Obs_end,y=~Rainfall_mm,color=~Station,type="scatter",mode="lines+markers",text=~tip,hoverinfo="text")
    } else { d <- a |> group_by(Obs_end) |> summarise(Mean=mean(Rainfall_mm),N=n(),.groups="drop")
    d$tip <- paste0("<b>Mean of reporting stations</b><br>Observation (period end): ",format(d$Obs_end,"%d %b %Y %H:%M",tz=LIVE_TZ)," IST<br>Mean rainfall: ",lv_mm(d$Mean)," = ",lv_cm(d$Mean),"<br>Stations: ",d$N,"<br>Source: TN-SMART daily report (24-h accumulations)")
    p <- plot_ly(d,x=~Obs_end,y=~Mean,type="scatter",mode="lines+markers",text=~tip,hoverinfo="text",line=list(color="#14866d"),marker=list(color="#14866d")) }
    p |> base_layout(xaxis=list(title="Observation time (end of 24-h period, IST)",gridcolor="#e8f0ed"),yaxis=list(title="24-hour rainfall (mm)",gridcolor="#e8f0ed",rangemode="tozero")) |> cfg()
  })
  output$lv_ts_note <- renderUI({ a <- lv_state()$archive; div(class="lv-note",paste0("Archive: ",n_distinct(a$Obs_end)," daily report(s) retrieved by this app",if(nrow(a)) paste0(" (",format(min(as.Date(a$Obs_end,tz=LIVE_TZ)),"%d %b %Y")," to ",format(max(as.Date(a$Obs_end,tz=LIVE_TZ)),"%d %b %Y"),")") else "",
                                                                                      ". All points are 24-hour accumulations from 08:30 to 08:30 IST; hourly, forecast and other series are not mixed in. Gaps are shown as gaps.")) })
  
  # ---- Selected station ----
  output$lv_selected <- renderUI({
    d <- lv_state()$rain$data; sel <- lv_sel()
    if(is.null(d)) return(div(class="lv-note","No station data retrieved."))
    if(is.null(sel) || !(sel %in% d$Label)) return(div(class="lv-note","Click a map marker, a bar in the ranked chart, or a table row to inspect a station."))
    r <- d[d$Label==sel,][1,]; m <- lv_state()$rain$meta
    tagList(tags$h4(style="margin:0 0 6px;font-size:1rem",r$Station),
            kv_dl(list(`Rainfall`=if(isTRUE(r$Valid)) lv_mmcm(r$Rainfall_mm) else "Unavailable",`Class`=if(isTRUE(r$Valid)) as.character(r$Class) else "Unavailable",
                       `Network`=r$Network,`Zone / ward`=paste0(ifelse(is.na(r$Zone),"n/a",r$Zone)," / ",ifelse(is.na(r$Ward),"n/a",r$Ward)),`Period`=r$Period,
                       `Observation time`=lv_fmt_t(m$obs_time),`Source`=r$Source,`Status`=lv_rain_st()$label,`Data check`=if(nzchar(r$Issue)) r$Issue else "valid reading")),
            { a <- lv_state()$archive; h <- filter(a,Station==r$Station); if(nrow(h)>1) div(class="lv-note",paste0(nrow(h)," retrieved daily reports; mean ",lv_mmcm(mean(h$Rainfall_mm))," per report.")) })
  })
  
  # ---- Weather observations ----
  output$lv_wx <- renderUI({
    lv_clock(); s <- lv_state(); st <- lv_status(s$wx,"obs"); d <- s$wx$data
    if(is.null(d)) return(div(class="lv-note",tags$b("Unavailable. "),if(!live_cfg$imd_enabled) tagList("IMD's current-weather API needs your server's public IP to be whitelisted by IMD. Official observations: ",tags$a(href=LV_LINKS$imd_chennai,target="_blank","IMD Chennai"),".") else paste0("No IMD observation retrieved",if(!is.na(s$wx$error)) paste0(": ",s$wx$error) else ".")))
    tagList(div(lv_chip(st)," ",span(class="lv-small","Source: IMD Current Weather API (observations; kept separate from TN-SMART rainfall)")),
            div(class="lv-tbl-wrap",tags$table(class="lv-tbl",tags$tr(lapply(c("Station","Observed (IST)","Temp","Humidity","Wind","Weather","Pressure"),tags$th)),
                                               lapply(seq_len(nrow(d)),function(i) { r <- d[i,]; tags$tr(tags$td(r$Station),tags$td(if(is.na(r$Obs_utc)) "Timestamp invalid" else lv_fmt_t(with_tz(r$Obs_utc,LIVE_TZ))),
                                                                                                         tags$td(if(r$Temp_ok) paste0(formatC(r$Temp,format="f",digits=1)," °C") else "Unavailable"),tags$td(if(r$RH_ok) paste0(round(r$RH)," %") else "Unavailable"),
                                                                                                         tags$td(paste0(ifelse(is.finite(r$WindSpeed),paste0(round(r$WindSpeed)," km/h"),"speed unavailable"),", ",lv_wind_dir(r$WindDir))),tags$td(lv_wx_desc(r$WxCode)),
                                                                                                         tags$td(ifelse(is.finite(r$MSLP),paste0(round(r$MSLP,1)," hPa"),"Unavailable"))) }))))
  })
  
  # ---- Official alerts ----
  output$lv_alerts <- renderUI({
    lv_clock(); s <- lv_state(); d <- s$warn$data; st <- lv_status(s$warn,"forecast")
    links <- div(class="lv-small",tags$a(href=LV_LINKS$imd_chennai,target="_blank","IMD Chennai warnings")," · ",tags$a(href=LV_LINKS$imd_ref,target="_blank","IMD API reference"))
    if(is.null(d)) return(tagList(div(class="lv-note",tags$b("Warning data unavailable. "),if(!live_cfg$imd_enabled) "IMD's API is not enabled for this server (IP whitelisting required). " else paste0("Retrieval failed",if(!is.na(s$warn$error)) paste0(": ",s$warn$error) else "",". "),
                                      "This is not confirmation that there are no warnings. Check the official IMD pages."),links))
    tagList(div(span(class="lv-chip fc",st$label)," ",span(class="lv-small",paste0("Issued ",d$Issue_date[1]," ",ifelse(is.na(d$Issue_utc_raw[1]),"",paste0(d$Issue_utc_raw[1]," UTC"))," · Source: IMD district-wise warnings · ",d$District[1]))),
            div(class="lv-tbl-wrap",tags$table(class="lv-tbl",tags$tr(lapply(c("Day","Level","Warning","Validity"),tags$th)),
                                               lapply(seq_len(nrow(d)),function(i) { r <- d[i,]; tags$tr(tags$td(paste0("Day ",r$Day)),
                                                                                                         tags$td(span(class="lv-sw",style=paste0("background:",ifelse(is.na(r$Color),"#fff",LV_WARN_COL[as.character(r$Color)]))),r$Level),tags$td(r$Description),tags$td(paste0("Day ",r$Day," of the 5-day outlook issued ",r$Issue_date))) }))),
            div(class="lv-note","Levels follow IMD's four-colour system (green: no advisory, yellow: be aware, orange: be prepared, red: take action). The dashboard never creates a warning from rainfall values. IMD's reference does not state the exact validity hours of each day."),links)
  })
  
  # ---- Table + downloads ----
  output$lv_table <- renderDT({
    d <- lv_state()$rain$data; validate(need(!is.null(d),"No station data retrieved."))
    t <- d |> transmute(Station,Network,Zone,Ward,`Rainfall (mm)`=Rainfall_mm,`Rainfall (cm)`=Rainfall_cm,Class=as.character(Class),`Data check`=ifelse(nzchar(Issue),Issue,"valid"))
    t$`Rainfall (mm)` <- round(t$`Rainfall (mm)`,1); t$`Rainfall (cm)` <- round(t$`Rainfall (cm)`,2)
    datatable(t,rownames=FALSE,selection="single",options=list(pageLength=10,scrollX=TRUE,columnDefs=list(list(targets=c(4,5),render=DT::JS("function(data,type){ if(type==='display' && (data===null || data==='NA')) return 'Unavailable'; return data; }")))))
  })
  observeEvent(input$lv_table_rows_selected, { d <- lv_state()$rain$data; i <- input$lv_table_rows_selected; if(!is.null(d) && length(i)) lv_sel(d$Label[i]) })
  output$lv_dl_readings <- downloadHandler(filename=function() paste0("ecoshield_live_rainfall_",Sys.Date(),".csv"),
                                           content=function(file) { d <- lv_state()$rain$data; if(is.null(d)) d <- tibble(Message="No data retrieved")
                                           else d <- d |> transmute(Station,Network,Zone,Ward,Rainfall_mm,Rainfall_cm,Class=as.character(Class),Data_check=Issue,Period,Observation_time_IST=lv_fmt_t(Obs_end[1]),Source); write_csv(d,file) })
  output$lv_dl_coords <- downloadHandler(filename=function() "station_coordinates_template.csv",
                                         content=function(file) { d <- lv_state()$rain$data; st <- if(is.null(d)) character() else unique(d$Station); write_csv(tibble(Station=st,Latitude=NA_real_,Longitude=NA_real_,Coordinate_Source=NA_character_),file,na="") })
  
  # ---- Methods ----
  output$lv_methods <- renderUI({
    tagList(
      tags$h4(style="font-size:.85rem;font-weight:800;color:#14866d;margin:4px 0","Data sources used"),
      tags$ul(
        tags$li(tags$b("TN-SMART Chennai rainfall: "),tags$a(href=LV_LINKS$tn,target="_blank",LV_LINKS$tn)," Public HTML report of the previous 24 h (08:30 IST to 08:30 IST) per station, in millimetres. No documented API, so the table is read from the page at most every ",live_cfg$refresh_min," min using an identifying user agent. Confirm TN-SMART/RIMES terms before heavy or commercial use; switch off with ECOSHIELD_TNSMART_ENABLED=false."),
        tags$li(tags$b("IMD public API: "),tags$a(href=LV_LINKS$imd_ref,target="_blank","reference"),". Used for current weather and Chennai district warnings only when ECOSHIELD_IMD_ENABLED=true. IMD whitelists server IPs and permits free non-commercial use with attribution (",tags$a(href=LV_LINKS$imd_whitelist,target="_blank","IP request form"),", ",tags$a(href=LV_LINKS$imd_apis,target="_blank","IMD API page"),")."),
        tags$li(tags$b("IMD Chennai centre: "),tags$a(href=LV_LINKS$imd_chennai,target="_blank",LV_LINKS$imd_chennai)," Portal only; no programmatic access was verified, so it is linked, not scraped.")),
      tags$h4(style="font-size:.85rem;font-weight:800;color:#14866d;margin:8px 0 4px","Methods"),
      tags$ul(
        tags$li("Rainfall: centimetres = millimetres ÷ 10. The highest single station reading is shown; station readings are never summed into a Chennai total, and repeated station entries are never averaged. Missing, malformed or conflicting readings are excluded and counted, never set to zero; genuine 0 mm readings are kept."),
        tags$li(paste0("Status: a daily 08:30 report is 'Recently updated' for ",live_cfg$fresh_daily_h," h after its period ends and 'Delayed' afterwards; sub-daily observations are 'Live / recently updated' for ",live_cfg$fresh_obs_h," h (heuristics). Warnings are always labelled Forecast. A failed refresh keeps the last valid data and flags it as possibly stale.")),
        tags$li("Rainfall classes use the ranges printed on the TN-SMART legend; they describe single readings and are not official warnings."),
        tags$li("Time series: built only from daily reports this app has retrieved and stored in the cache folder; it grows by one day per day of operation.")),
      tags$h4(style="font-size:.85rem;font-weight:800;color:#14866d;margin:8px 0 4px","Limitations"),
      tags$ul(
        tags$li("TN-SMART's own station list publishes coordinates only for the Revenue Department gauges (11 in Chennai, stored in station_coordinates.csv, matched to the daily report by station name). The Greater Chennai Corporation ward gauges have no published coordinates, so they appear in the chart and table but not on the map; none are guessed."),
        tags$li("Weather and warnings are unavailable until IMD whitelists the server IP. The IMD field names follow IMD's published reference and were not verified against live responses."),
        tags$li("The TN-SMART page layout is not a stable interface; the parser finds the station table by its structure rather than a fixed position, but if the layout changes the dashboard shows 'Data unavailable' (or the last valid data labelled stale) rather than wrong numbers. The source does not state a time zone; 08:30 is interpreted as Chennai local time (IST). Station-name uniqueness is not documented by the source, so repeated names are never merged or averaged.")))
  })
}
shinyApp(ui,server)