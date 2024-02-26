to_tag = function(key, value) {
  if (value == 0) return("")
  sprintf('%s="%s"', key, value)
}

# row:
# cell id
# fill colour = #ffffff
# parent id
# width
# height
#
drawio_row = function(cell_id, parent_id, x, y, width, height, fill_colour = c("#ffffff", "#EAF2D3", "#A7C942")) {
  fill_colour = match.arg(fill_colour)
  fmt =
'
                <mxCell id="%s" style="shape=tableRow;horizontal=0;startSize=0;swimlaneHead=0;swimlaneBody=0;top=0;left=0;bottom=0;right=0;dropTarget=0;collapsible=0;recursiveResize=0;expand=0;fontStyle=0;strokeColor=inherit;fillColor=%s;" vertex="1" parent="%s">
                    <mxGeometry %s %s width="%s" height="%s" as="geometry"/>
                </mxCell>
'
  sprintf(fmt, cell_id, fill_colour, parent_id, to_tag("x", x), to_tag("y", y), width, height)
}

# cell id
# cell entry
# fill colour #A7C942
# parent_id
# width
# height
drawio_cell = function(cell_id, parent_id, cell_entry, x, y, width, height, fill_colour = c("#ffffff", "#EAF2D3", "#A7C942"), font_colour = "#FFFFFF", font_style = 0) {
  fill_colour = match.arg(fill_colour)
  fmt =
'
                <mxCell id="%s" value="%s" style="connectable=0;recursiveResize=0;strokeColor=inherit;fillColor=%s;align=center;fontStyle=%s;fontColor=%s;html=1;" vertex="1" parent="%s">
                    <mxGeometry %s %s width="%s" height="%s" as="geometry">
                        <mxRectangle width="%s" height="%s" as="alternateBounds"/>
                    </mxGeometry>
                </mxCell>
'
  sprintf(fmt, cell_id, cell_entry, fill_colour, font_style, font_colour, parent_id, to_tag("x", x), to_tag("y", y), width, height, width, height)
}

drawio_box = function(cell_id, parent_id, box_entry, x, y, width, height) {
  # box:
# cell_id
# box_entry
# parent_id
# x
# y
# box_width
# box_height
fmt = 
'
                <mxCell id="%s" value="%s" style="rounded=0;whiteSpace=wrap;html=1;" parent="%s" vertex="1">
                    <mxGeometry %s %s width="%s" height="%s" as="geometry"/>
                </mxCell>
'
  sprintf(fmt, cell_id, box_entry, parent_id, to_tag("x", x), to_tag("y", y), width, height)
}

# assets:
# cell id
# fill colour #A7C942
# parent id
# width
# height
drawio_assets = function(cell_id, parent_id, x, y, width, height, fill_colour = c("#ffffff", "#EAF2D3", "#A7C942")) {
  fill_colour = fill_colour[1]
  fmt =
'
                <mxCell id="%s" value="Assets" style="childLayout=tableLayout;recursiveResize=0;strokeColor=#98bf21;fillColor=%s;shadow=0;rounded=0;glass=0;" parent="%s" vertex="1">
                    <mxGeometry %s %s width="%s" height="%s" as="geometry"/>
                </mxCell>
'
  sprintf(fmt, cell_id, fill_colour, parent_id, to_tag("x", x), to_tag("y", y), width, height)
}

drawio_table_wrap = function(...) {
  fmt =
'
        <mxGraphModel dx="667" dy="497" grid="1" gridSize="10" guides="1" tooltips="1" connect="1" arrows="1" fold="1" page="1" pageScale="1" pageWidth="700" pageHeight="1400" math="1" shadow="0">
            <root>
                <mxCell id="0"/>
                <mxCell id="1" parent="0"/>
                %s
            </root>
        </mxGraphModel>
'
  sprintf(fmt, paste0(..., collapse = "\\n"))
}


drawio_graph = function(flows, x = 0, y = 0) {
  # drawio_box = function(cell_id, parent_id, box_entry, x, y, width, height)
}

#' @describeIn drawio_table Place several `draw.io` tables in a single file.
#' Each table is provided as a named list of the arguments to pass to
#' `drawio_table`.
#' @export
drawio_tables = function(..., file_name = "") {
  input = list(...)
  file_name = get_file_name(input, file_name)
  output = list()
  id_init = 2
  for (i in seq_along(input)) {
    input[[i]]$id_init = id_init
    output[[i]] = do.call(drawio_table, input[[i]])
    id_init = output[[i]]$cell_id + 1
  }
  xml_list = lapply(output, getElement, "xml")
  xml = do.call(drawio_table_wrap, xml_list)
  save_if(xml, file_name)
  output
}

#' Draw IO Table
#'
#' Create a `draw.io` file with a table giving a data frame.
#'
#' @param data_frame The data frame to represent as a `draw.io` table.
#' @param x Horizontal position of the table.
#' @param y Vertical position of the table.
#' @param cell_width Width of table cells.
#' @param cell_height Height of table cells.
#' @param file_name Optional name of file. If missing, the xml will be returned.
#' @param id_init Initial cell identifier. This is not usually need by users.
#'
#' @export
drawio_table = function(data_frame
    , x = 0, y = 0
    , cell_width = 90, cell_height = 20
    , file_name = ""
    , id_init = 2
  ) {
  table_width = ncol(data_frame) * cell_width
  table_height = (nrow(data_frame) + 1) * cell_height
  table_args = list(
      drawio_assets(cell_id = id_init, parent_id = id_init - 1, x = x, y = y, width = table_width, height = table_height, fill_colour = "#ffffff")
    , drawio_row(cell_id = id_init + 1, parent_id = id_init, x = 0, y = 0, width = table_width, height = cell_height, fill_colour = "#ffffff")
  )
  for (i in seq_along(data_frame)) {
    cell_id = id_init + 1 + i
    x_use = (i - 1) * cell_width
    table_args = append(table_args
      , drawio_cell(cell_id = cell_id, parent_id = id_init + 1, cell_entry = names(data_frame)[i], x = x_use, y = 0, width = cell_width, height = cell_height, fill_colour = "#A7C942", font_colour = "#ffffff", font_style = 1)
    )
  }
  for (i in seq_len(nrow(data_frame))) {
    cell_id = cell_id + 1
    y_use = i * cell_height
    table_args = append(table_args
      , drawio_row(cell_id = cell_id, parent_id = id_init, x = 0, y = y_use, width = table_width, height = cell_height, fill_colour = "#ffffff")
    )
    parent_id = cell_id
    fill_colour = c("#EAF2D3", "#ffffff")[1 + (i %% 2)]
    for (j in seq_along(data_frame)) {
      cell_id = cell_id + 1
      x_use = (j - 1) * cell_width
      table_args = append(table_args
        , drawio_cell(cell_id = cell_id, parent_id = parent_id, cell_entry = data_frame[i, j], x = x_use, y = 0, width = cell_width, height = cell_height, fill_colour = fill_colour, font_colour = "030303", font_style = 0)
      )
    }
  }
  xml = do.call(paste0, c(table_args, collapse = "\\n"))
  save_if(xml, file_name)
  structure(
    list(data_frame = data_frame, xml = xml, cell_id = cell_id),
    class = "drawio_table"
  )
}

#' @export
print.drawio_table = function(x, ...) {
  cat("Drawio Table Object for the following data frame:\n")
  print(x$data_frame)
}

save_if = function(xml, file_name) {
  try(file.create(file_name, showWarnings = FALSE), silent = TRUE)
  file_exists = isTRUE(try(file.exists(file_name), silent = TRUE))
  if (file_exists) cat(xml, file = file_name)
}

get_file_name = function(input, file_name) {
  input = append(input, list(list(file_name = file_name)))
  file_names = (input
    |> lapply(getElement, "file_name")
    |> Filter(f = function(x) {isTRUE((nchar(x) > 0L)) & (!is.null(x))})
    |> unique()
  )
  if (length(file_names) == 1L) {
    file_name = unlist(file_names, use.names = FALSE)
  } else if (length(file_names) > 1L) {
    stop("Conflicting filenames")
  } else {
    file_name = ""
  }
  return(file_name)
}

if (FALSE) {
  library(macpan2helpers)
  tmp_file = tempfile(fileext = ".drawio")
  #tmp_file = "../macpan2/misc/diagrams/drawio-templates/test.drawio"
  
  f = read_frame(file.path("../macpan2/inst/model_library/sir/flows.csv"))
  macpan2helpers:::topological_sort_engine(f)
  
  drawio_tables(list(data.frame(x = "$$\\lambda$$"), x = 0, y = 0), file_name = tmp_file)
  system2("open", c("-a", '"Visual Studio Code"', tmp_file))
  
  #sir_vax = Compartmental(system.file("starter_models", "sir_vax", package = "macpan2"))
  drawio_tables(
    list(v$filter("state", .wrt = "Vec")$frame(), x = 0, y = 0),
    list(v$filter("flow", .wrt = "Vec")$frame(), x = 100, y = 100),
    file_name = tmp_file
  )
  system2("open", c("-a", '"Visual Studio Code"', tmp_file))
  
  system2("/Applications/draw.io.app/Contents/MacOS/draw.io", c("-x", tmp_file))
  system2("open", paste0(tools::file_path_sans_ext(tmp_file), ".pdf"))
}



# flow
# cell_id
#
fmt = '
                <mxCell id="%s" style="edgeStyle=none;html=1;exitX=1;exitY=0.5;exitDx=0;exitDy=0;entryX=0;entryY=0.5;entryDx=0;entryDy=0;" parent="%s" source="%s" target="%s" edge="1">
                    <mxGeometry relative="1" as="geometry"/>
                </mxCell>
                <mxCell id="%s" value="infection" style="rounded=0;whiteSpace=wrap;html=1;strokeColor=none;" parent="1" vertex="1">
                    <mxGeometry x="120" y="50" width="50" height="20" as="geometry"/>
                </mxCell>
                <mxCell id="%s" style="edgeStyle=none;html=1;exitX=1;exitY=0.5;exitDx=0;exitDy=0;entryX=0;entryY=0.5;entryDx=0;entryDy=0;" parent="1" source="43" target="40" edge="1">
                    <mxGeometry relative="1" as="geometry"/>
                </mxCell>
'
