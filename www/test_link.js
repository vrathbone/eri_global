var customHref = function(tabName) {
	var navList = document.querySelectorAll(".navbar-nav a");
	for (const link of navList) {
	  if (link.getAttribute("data-value") == tabName) {
	  	link.click();
	  	setTimeout(() => {
	  	  window.scrollTo(0, 0);
	  	}, 0);
	  	break;
		}
	}
};