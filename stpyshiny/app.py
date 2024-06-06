import io
from pathlib import Path
import faicons
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
from shinywidgets import output_widget, render_plotly
from shiny import App, Inputs, Outputs, Session, reactive, render, ui, req

www_dir = Path(__file__).parent / "www"

data_src_path = Path(__file__).parent / "df_plot.csv"
df = pd.read_csv(data_src_path, sep=",")

app_ui = ui.page_fillable(
    ui.tags.link(
        rel="stylesheet",
        href="https://fonts.googleapis.com/css?family=Roboto"
    ),
    ui.div(
        ui.img(src="logo.PNG", style="width:100%; object-fit:contain;"), 
        style="width:200px; padding:0;"
    ), 
    ui.layout_columns(
        ui.card(
            ui.card_header(
                "Data source",
                ui.popover(
                    faicons.icon_svg("circle-info"),
                    ui.input_action_button("add", "Show data souce"),
                    placement="left",
                ),
                style="color:white; background:#2A2A2A !important;", 
                class_="d-flex justify-content-between align-items-center",
            ),
            ui.output_data_frame("file_list"),
        ),
        ui.output_ui("log_output", fill=True, fillable=True),
        ui.include_css(www_dir / "style.css"),
        col_widths=[6, 6],
    ),
    ui.p(
        ui.markdown("_Check out the [documentation](https://stewartli.github.io/adawf/)_"),
        style="font-size: 0.7rem;"
    ),
)

def server(input: Inputs, output: Outputs, session: Session):
    @render.data_frame
    def file_list():
        return render.DataTable(df, selection_mode="rows")

    @reactive.effect
    def sim_logs():
        req(input.add())
        ui.update_action_button("add", label="df_plot.csv", icon="🔗")

    @render.ui
    def log_output():
        idx = input.file_list_selected_rows()
        if not idx:
            return ui.card(
                ui.card_header("Selected"),
                ui.div(faicons.icon_svg("circle-play"), ui.p("Select data on the left via Ctrl key", style="display:inline-block;")),
                ui.div(faicons.icon_svg("circle-play"), ui.p("Download the selected data on the right", style="display:inline-block;")),
                ui.div(faicons.icon_svg("circle-play"), ui.p("Check the trend on the right", style="display:inline-block;")),
            )
        return ui.card(
            ui.card_header(
                "Selected",
                ui.download_link(
                    "download", "", icon=faicons.icon_svg("download")
                ),
                style="color:white; background:#2A2A2A !important;", 
                class_="d-flex justify-content-between align-items-center",
            ),
            ui.output_data_frame("data_grid"),
            output_widget("mov")
        )
    
    @reactive.calc
    def selected_file():
        data_selected = file_list.data_view(selected=True)
        req(not data_selected.empty)
        car_name = data_selected["subaccount"]
        return df[df["subaccount"].isin(car_name)]

    @render.data_frame
    def data_grid():
        data_show = selected_file()
        res = data_show.groupby("subaccount").apply(lambda gdf: gdf.iloc[1])
        return render.DataTable(res, width='fit-content', height='200px')

    @render.download(filename="logs.csv")
    def download():
        df_csv = selected_file()
        with io.StringIO() as buf:
            df_csv.to_csv(buf)
            yield buf.getvalue().encode()

    @render_plotly
    def mov():
        df_res = selected_file()
        fig1 = px.line(df_res, x='fcf', y='fcfval', color='subaccount')
        fig2 = px.scatter(df_res, x='fcf', y='fcfval', color='subaccount').update_traces(showlegend=False)
        fig3 = go.Figure(data=fig1.data + fig2.data)
        fig3.update_layout(
            legend=dict(orientation="h", yanchor="bottom", y=1.02, xanchor="right", x=1),
            hovermode="x unified",
            paper_bgcolor="rgba(0,0,0,0)",
            plot_bgcolor="rgba(0,0,0,0)",
        )
        return fig3

app = App(app_ui, server, static_assets=www_dir)
