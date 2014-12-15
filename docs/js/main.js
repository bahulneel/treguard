$(function () {
    $('sequence').each(function (index, value) {
        var e = $(value);
        var diagram = Diagram.parse(e.text());
        e.html('');
        diagram.drawSVG(value);
    });

    $('flowchart').each(function (index, value) {
        var e = $(value);
        var diagram = flowchart.parse(e.text());
        e.html('');
        diagram.drawSVG(value);
    });
});
