(function() {
    $('div.image > img').replaceWith(function() {
        var $this = this;

        return '<a href="' + $this.src + '"><img alt="' + $this.alt + '" src="' + $this.src + '" /></a>';
    });
})();
